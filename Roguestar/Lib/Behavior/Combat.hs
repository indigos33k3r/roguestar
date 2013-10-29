{-# LANGUAGE PatternGuards, FlexibleContexts, ScopedTypeVariables #-}

module Roguestar.Lib.Behavior.Combat
    (AttackModel,
     meleeAttackModel,
     rangedAttackModel,
     resolveAttackChain,
     executeAttackChain)
    where

import Roguestar.Lib.DB
import Roguestar.Lib.Core.Monster
import Roguestar.Lib.Data.MonsterData
import Roguestar.Lib.Tool
import Roguestar.Lib.Data.ToolData
import Control.Monad.Error
import Control.Monad.Random
import Control.Monad.Reader
import Roguestar.Lib.Data.FacingData
import Data.Maybe
import Roguestar.Lib.Utility.Contact
import Roguestar.Lib.Utility.DetailedLocation
import Data.List as List

data AttackModel =
    RangedAttackModel MonsterRef ToolRef Device
  | MeleeAttackModel MonsterRef ToolRef Device
  | UnarmedAttackModel MonsterRef

attacker :: AttackModel -> MonsterRef
attacker (RangedAttackModel attacker_ref _ _) = attacker_ref
attacker (MeleeAttackModel attacker_ref _ _) = attacker_ref
attacker (UnarmedAttackModel attacker_ref) = attacker_ref

weapon :: AttackModel -> Maybe ToolRef
weapon (RangedAttackModel _ weapon_ref _) = Just weapon_ref
weapon (MeleeAttackModel _ weapon_ref _) = Just weapon_ref
weapon (UnarmedAttackModel {}) = Nothing

interactionMode :: AttackModel -> MonsterInteractionMode
interactionMode (RangedAttackModel {}) = Ranged
interactionMode (MeleeAttackModel {}) = Melee
interactionMode (UnarmedAttackModel {}) = Unarmed

-- | Get the attack model for a creature, based on whatever tool the creature is holding.
-- This will fail if the creature is holding anything other than a weapon.
attackModel :: (DBReadable db) => MonsterRef -> db AttackModel
attackModel attacker_ref =
    do m_tool_ref <- getWielded attacker_ref
       case m_tool_ref of
           Nothing -> return $ UnarmedAttackModel attacker_ref
           Just tool_ref ->
               do tool <- asks $ getTool tool_ref
                  case tool of
                      DeviceTool Gun device -> return $ RangedAttackModel attacker_ref tool_ref device
                      DeviceTool Sword device -> return $ MeleeAttackModel attacker_ref tool_ref device
                      _ -> throwError $ DBErrorFlag ToolIs_Innapropriate

-- | Get an appropriate melee attack model for a creature, based on whatever tool the creature is holding.
-- This will fail if the creature is holding anything other than a suitable melee weapon (allows unarmed strike).
meleeAttackModel :: (DBReadable db) => MonsterRef -> db AttackModel
meleeAttackModel attacker_ref =
    do attack_model <- attackModel attacker_ref
       case interactionMode attack_model `elem` [Melee,Unarmed] of
           True -> return attack_model
           _ -> throwError $ DBErrorFlag ToolIs_Innapropriate

-- | Get an appropriate ranged attack model for a creature, based on whatever tool the creature is holding.
-- This will fail if the creature is holding anything other than a suitable ranged or splash weapon.
rangedAttackModel :: (DBReadable db) => MonsterRef -> db AttackModel
rangedAttackModel attacker_ref =
    do attack_model <- attackModel attacker_ref
       case interactionMode attack_model `elem` [Ranged,Splash] of
           True -> return attack_model
           _ -> throwError $ DBErrorFlag ToolIs_Innapropriate

data WeaponActivationOutcome =
    WeaponExplodes MonsterRef ToolRef Integer
  | WeaponMalfunctions MonsterRef ToolRef Integer
  | WeaponFunctions

data AttackOutcome =
    AttackMisses MonsterRef (Maybe ToolRef)
  | AttackHits Integer

numberOfHits :: AttackOutcome -> Integer
numberOfHits (AttackMisses {}) = 0
numberOfHits (AttackHits n) = n

data DamageOutcome =
    DamageInflicted MonsterRef (Maybe ToolRef) MonsterRef Integer
  | DamageDisarms MonsterRef MonsterRef ToolRef Integer
  | DamageSunders MonsterRef ToolRef MonsterRef ToolRef Integer

isDisarmingBlow :: DamageOutcome -> Bool
isDisarmingBlow (DamageInflicted {}) = False
isDisarmingBlow _ = True

weighWeaponActivationOutcomes :: (DBReadable db) => AttackModel -> db (WeightedSet WeaponActivationOutcome)
weighWeaponActivationOutcomes attack_model =
    do attack_rating <- getMonsterAbilityScore (AttackSkill $ interactionMode attack_model) $ attacker attack_model
       damage_rating <- getMonsterAbilityScore (DamageSkill $ interactionMode attack_model) $ attacker attack_model
       case weapon attack_model of
           Nothing -> return $ unweightedSet [WeaponFunctions]
           Just weapon_ref ->
               do device_weight <- toolValue weapon_ref
                  return $ weightedSet
                      [(max 0 device_weight - attack_rating - damage_rating,
                        WeaponExplodes (attacker attack_model) weapon_ref device_weight),
                       (device_weight, WeaponMalfunctions (attacker attack_model) weapon_ref 1),
                       (attack_rating+damage_rating, WeaponFunctions)]

weighAttackOutcomes :: (DBReadable db) => AttackModel -> MonsterRef -> db (WeightedSet AttackOutcome)
weighAttackOutcomes attack_model defender =
    do attack_rating <- getMonsterAbilityScore (AttackSkill $ interactionMode attack_model) $ attacker attack_model
       defense_rating <- getMonsterAbilityScore (DefenseSkill $ interactionMode attack_model) $ defender
       return $ weightedSet
           [{- TODO: put a "riposte" counter attack if the flub by too much? -- not spending time on it now -}
            (defense_rating, AttackMisses (attacker attack_model) (weapon attack_model)),
            (attack_rating, AttackHits 1),
            (max 0 $ attack_rating - defense_rating, AttackHits $ max 1 $ attack_rating `div` (max 1 defense_rating))]

weighDamageOutcomes :: (DBReadable db) => AttackModel -> MonsterRef -> Maybe ToolRef -> db (WeightedSet DamageOutcome)
weighDamageOutcomes attack_model defender m_shield =
    do damage_rating <- getMonsterAbilityScore (DamageSkill $ interactionMode attack_model) $ attacker attack_model
       damage_reduction_rating <- getMonsterAbilityScore (DamageReductionTrait $ interactionMode attack_model) $ defender
       let damageInflicted = DamageInflicted (attacker attack_model) (weapon attack_model) defender
       shield_weight <- maybe (return 0) toolValue m_shield
       weapon_weight <- maybe (return 0) toolValue $ weapon attack_model
       return $ weightedSet
           [(damage_reduction_rating, damageInflicted $ max 1 $ damage_rating - damage_reduction_rating),
            (damage_rating, damageInflicted damage_rating),
            (max 0 $ damage_reduction_rating - damage_rating, damageInflicted 0),
            (max 0 $ damage_rating - damage_reduction_rating,
                case (weapon attack_model, m_shield) of
                    (_       ,Nothing    ) -> damageInflicted $ damage_rating + weapon_weight
                    (Nothing ,Just shield) -> DamageDisarms
                                                  (attacker attack_model)
                                                  defender
                                                  shield
                                                  (max 0 $ damage_rating - damage_reduction_rating - shield_weight)
                    (Just weapon_ref, Just shield_ref) -> DamageSunders
                                                          (attacker attack_model)
                                                          weapon_ref
                                                          defender
                                                          shield_ref
                                                          (max 0 $ damage_rating + weapon_weight - shield_weight))]

data AttackChainOutcome = AttackChainOutcome {
    _chain_weapon_outcome :: WeaponActivationOutcome,
    _chain_attack_outcome :: AttackOutcome,
    _chain_damage_outcome :: [DamageOutcome] }

resolveAttackChain :: forall db. (MonadRandom db, DBReadable db) => AttackModel -> Either Facing MonsterRef -> db AttackChainOutcome
resolveAttackChain attack_model e_face_defender =
    do m_defender_ref <- case e_face_defender of
           Right defender_ref -> return $ Just defender_ref
           Left face -> liftM (listToMaybe . List.map asChild . mapLocations) $ findContacts (contactMode $ interactionMode attack_model) (attacker attack_model) face
       weapon_outcome <- weightedPickM =<< weighWeaponActivationOutcomes attack_model
       (attack_outcome, damage_outcome_list) <- case m_defender_ref of
           Nothing -> return (AttackMisses (attacker attack_model) (weapon attack_model), [])
           Just defender ->
               do (attack_outcome :: AttackOutcome) <- weightedPickM =<< weighAttackOutcomes attack_model defender
                  m_shield <- getWielded defender
                  let figureDamage :: [DamageOutcome] -> Integer -> db [DamageOutcome]
                      figureDamage outcomes _ =
                          do damage_outcome <- weightedPickM =<< weighDamageOutcomes attack_model defender
                                                   (if any isDisarmingBlow outcomes
                                                    then Nothing else m_shield)
                             return $ damage_outcome:outcomes
                  damage_outcome_list <- foldM figureDamage [] [1..numberOfHits attack_outcome]
                  return (attack_outcome, reverse $ damage_outcome_list)
       return $ AttackChainOutcome weapon_outcome attack_outcome damage_outcome_list

executeAttackChain :: AttackChainOutcome -> DB ()
executeAttackChain (AttackChainOutcome (WeaponExplodes attacker_ref tool_ref damage) _ _) =
    do injureMonster damage attacker_ref
       dbPushSnapshot $ WeaponExplodesEvent attacker_ref tool_ref
       deleteTool tool_ref
executeAttackChain (AttackChainOutcome (WeaponMalfunctions attacker_ref tool_ref damage) _ _) =
    do injureMonster damage attacker_ref
       _ <- move tool_ref =<< dropTool tool_ref
       dbPushSnapshot $ WeaponOverheatsEvent attacker_ref tool_ref
       return ()
executeAttackChain (AttackChainOutcome _ (AttackMisses attacker_ref m_tool_ref) _) =
    do dbPushSnapshot $ MissEvent attacker_ref m_tool_ref
executeAttackChain (AttackChainOutcome _ _ damages) =
    do mapM_ executeDamage damages

executeDamage :: DamageOutcome -> DB ()
executeDamage (DamageInflicted attacker_ref m_tool_ref defender_ref damage) =
    do injureMonster damage defender_ref
       dbPushSnapshot $ AttackEvent attacker_ref m_tool_ref defender_ref
executeDamage (DamageDisarms attacker_ref defender_ref dropped_tool damage) =
    do injureMonster damage defender_ref
       dbPushSnapshot $ DisarmEvent attacker_ref defender_ref dropped_tool
       _ <- move dropped_tool =<< dropTool dropped_tool
       return ()
executeDamage (DamageSunders attacker_ref weapon_ref defender_ref sundered_tool damage) =
    do injureMonster damage defender_ref
       dbPushSnapshot $ SunderEvent attacker_ref weapon_ref defender_ref sundered_tool
       deleteTool sundered_tool


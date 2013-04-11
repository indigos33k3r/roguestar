module Roguestar.Lib.Data.PlayerState
    (PlayerState(..),
     SnapshotEvent(..),
     HasParticipants(..),
     GameOverReason(..))
    where

import Roguestar.Lib.Data.LocationData
import Roguestar.Lib.Data.MonsterData
import Roguestar.Lib.Data.TravelData
import Roguestar.Lib.Data.PersistantData
import Data.Maybe

data PlayerState =
    SpeciesSelectionState (Maybe Monster)
  | PlayerMonsterTurn MonsterRef
  | SnapshotEvent SnapshotEvent
  | GameOver GameOverReason
     deriving (Read,Show,Eq)

data GameOverReason = PlayerIsDead | PlayerIsVictorious
     deriving (Read,Show,Eq)

data SnapshotEvent =
    AttackEvent {
        attack_event_source_creature :: MonsterRef,
        attack_event_source_weapon :: Maybe ToolRef,
        attack_event_target_creature :: MonsterRef }
  | MissEvent {
        miss_event_creature :: MonsterRef,
        miss_event_weapon :: Maybe ToolRef }
  | KilledEvent {
        killed_event_creature :: MonsterRef }
  | WeaponOverheatsEvent {
        weapon_overheats_event_creature :: MonsterRef,
        weapon_overheats_event_weapon :: ToolRef }
  | WeaponExplodesEvent {
        weapon_explodes_event_creature :: MonsterRef,
        weapon_explodes_event_weapon :: ToolRef }
  | DisarmEvent {
        disarm_event_source_creature :: MonsterRef,
        disarm_event_target_creature :: MonsterRef,
        disarm_event_target_tool :: ToolRef }
  | SunderEvent {
        sunder_event_source_creature :: MonsterRef,
        sunder_event_source_weapon :: ToolRef,
        sunder_event_target_creature :: MonsterRef,
        sunder_event_target_tool :: ToolRef }
  | TeleportEvent {
        teleport_event_creature :: MonsterRef }
  | SpawnEvent {
        spawn_event_creature :: MonsterRef }
  | ClimbEvent {
        climb_event_direction :: ClimbDirection,
        climb_event_creature :: MonsterRef }
  | HealEvent {
        heal_event_creature :: MonsterRef }
  | ExpendToolEvent {
        expend_tool_event_tool :: ToolRef }
  | BumpEvent {
        bump_event_creature :: MonsterRef,
        bump_event_new_level :: Maybe Integer,
        bump_event_new_class :: Maybe CharacterClass }
            deriving (Read,Show,Eq)

class HasParticipants a where
    subjectOf :: a -> Maybe MonsterRef
    targetOf :: a -> Maybe MonsterRef
    participantsOf :: a -> [MonsterRef]
    participantsOf a = catMaybes $ [subjectOf a, targetOf a]

instance HasParticipants PlayerState where
    subjectOf (SpeciesSelectionState {}) = Nothing
    subjectOf (PlayerMonsterTurn x) = Just x
    subjectOf (SnapshotEvent x) = subjectOf x
    subjectOf (GameOver {}) = Nothing
    targetOf (SpeciesSelectionState {}) = Nothing
    targetOf (PlayerMonsterTurn x) = Just x
    targetOf (SnapshotEvent x) = targetOf x
    targetOf (GameOver {}) = Nothing

instance HasParticipants SnapshotEvent where
    subjectOf event = case event of
        AttackEvent { attack_event_source_creature = attacker_ref } -> Just attacker_ref
        MissEvent { miss_event_creature = attacker_ref } -> Just attacker_ref
        WeaponOverheatsEvent { weapon_overheats_event_creature = attacker_ref } -> Just attacker_ref
        WeaponExplodesEvent { weapon_explodes_event_creature = attacker_ref } -> Just attacker_ref
        KilledEvent killed_ref -> Just killed_ref
        DisarmEvent { disarm_event_source_creature = attacker_ref } -> Just attacker_ref
        SunderEvent { sunder_event_source_creature = attacker_ref } -> Just attacker_ref
        TeleportEvent { teleport_event_creature = creature_ref } -> Just creature_ref
        SpawnEvent { spawn_event_creature = creature_ref } -> Just creature_ref
        HealEvent { heal_event_creature = creature_ref } -> Just creature_ref
        ClimbEvent { climb_event_creature = creature_ref } -> Just creature_ref
        BumpEvent { bump_event_creature = creature_ref } -> Just creature_ref
        ExpendToolEvent {} -> Nothing
    targetOf event = case event of
        AttackEvent { attack_event_target_creature = target_ref } -> Just target_ref
        MissEvent {} -> Nothing
        WeaponOverheatsEvent {} -> Nothing
        WeaponExplodesEvent {} -> Nothing
        KilledEvent {} -> Nothing
        DisarmEvent { disarm_event_target_creature = target_ref } -> Just target_ref
        SunderEvent { sunder_event_target_creature = target_ref } -> Just target_ref
        TeleportEvent {} -> Nothing
        SpawnEvent {} -> Nothing
        HealEvent {} -> Nothing
        ClimbEvent {} -> Nothing
        BumpEvent {} -> Nothing
        ExpendToolEvent {} -> Nothing


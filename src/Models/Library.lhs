\section{The Model Library}

\begin{code}
module Models.Library
    (Library,
     newLibrary,
     lookupModel)
    where
    
import Quality
import Models.LibraryData
import Models.Terrain
import Models.QuestionMark
import Data.Map as Map
import RSAGL.Model
import RSAGL.QualityControl
import Control.Concurrent
import RSAGL.Bottleneck
import Control.Monad
import System.IO
import Models.Encephalon
import Models.PhaseWeapons
import Models.MachineParts

toModel :: LibraryModel -> Quality -> Modeling ()
toModel (TerrainTile s) = const $ terrainTile s
toModel QuestionMark = const $ question_mark
toModel NullModel = const $ return ()
toModel Encephalon = encephalon
toModel PhasePistol = phase_pistol
toModel MachineArmLower = machine_arm_lower
toModel MachineArmUpper = machine_arm_upper

all_library_models :: [LibraryModel]
all_library_models =
    Prelude.map TerrainTile known_terrain_types ++
    [QuestionMark, NullModel,
     Encephalon,
     PhasePistol,
     MachineArmLower,
     MachineArmUpper]

data Library = Library 
    Bottleneck
    (MVar (Map LibraryModel (QualityCache Quality IntermediateModel)))

newLibrary :: IO Library
newLibrary = 
    do lib <- liftM2 Library newBottleneck (newMVar Map.empty)
       mapM_ (\x -> do hPutStrLn stderr ("Preloading model: " ++ show x)
                       lookupModel lib x Poor) all_library_models
       return lib

lookupModel :: Library -> LibraryModel -> Quality -> IO IntermediateModel
lookupModel (Library bottleneck lib) lm q =
    do lib_map <- takeMVar lib
       m_qo <- return $ Map.lookup lm lib_map
       case m_qo of
           Just qo -> do putMVar lib lib_map
	                 getQuality qo q
	   Nothing -> do qo <- newQuality bottleneck parIntermediateModel (\q' -> toIntermediateModel (qualityToVertices q') (toModel lm q')) [Bad,Poor,Good,Super]
                         putMVar lib $ insert lm qo lib_map
			 getQuality qo q
\end{code}

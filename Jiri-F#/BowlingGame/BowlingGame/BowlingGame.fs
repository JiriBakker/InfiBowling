namespace BowlingGame
open System.Collections.Generic;
open System;

type InvalidPinCountException(message) =
   inherit Exception(message)

type InvalidFrameNumberException(message) =
   inherit Exception(message)

type GameNotActiveException(message) =
   inherit Exception(message)
   

type private FrameResult = Normal | Spare | Strike

type private GameState = ReadyForFirstThrow | ReadyForSecondThrow | GameFinished

type Game() =

    let isValidFrameNr   frameNr = (frameNr >= 1 && frameNr <= 12)
    let isRegularFrameNr frameNr = (frameNr >= 1 && frameNr <= 10)

    let isFirstThrow gameState = 
        (gameState = ReadyForFirstThrow)

    let toggleFirstThrow gameState =
        match gameState with
            | ReadyForFirstThrow  -> ReadyForSecondThrow
            | ReadyForSecondThrow -> ReadyForFirstThrow
            | GameFinished        -> raise (GameNotActiveException("Game has ended"))        

    let assertValidFrameNr frameNr = 
        if not (isValidFrameNr frameNr) then raise (InvalidFrameNumberException("Frame number should be higher than or equal to 1 and less than or equal to 12"))         

    let assertValidFrameScore frameScore =
        if frameScore > 10 then raise (InvalidPinCountException("Pin count is higher than number of currently remaining pins in frame"))

    let getFrameScore frameNr frameScores =
        match Map.tryFind frameNr frameScores with
        | Some score -> score
        | None -> 0
        
    
    let getFrameResult frameNr frameResults =
        match Map.tryFind frameNr frameResults with
        | Some result -> result
        | None -> Normal

    let updatePreviousFrameScores frameNr pins frameResults frameScores gameState =        
        seq {
            if frameNr > 1 then            
                let previousResult = getFrameResult (frameNr - 1) frameResults

                if (previousResult = Spare && isFirstThrow gameState) || previousResult = Strike then
                    yield (frameNr - 1, (getFrameScore(frameNr - 1) frameScores) + pins)
            
                if frameNr > 2 then
                    if (getFrameResult(frameNr - 2) frameResults) = Strike && previousResult = Strike then
                        yield (frameNr - 2, (getFrameScore(frameNr - 2) frameScores) + pins)    
        }

    let previousFrameResult currentFrameNr = 
        getFrameResult (currentFrameNr - 1)    

    let checkIfGameFinished currentFrameNr gameState frameResults =
        match (currentFrameNr, previousFrameResult currentFrameNr frameResults) with
            | (13, _)
            | (12, Normal)
            | (11, Normal) -> GameFinished
            | _ -> gameState

    let checkGameIsActive gameState =
        if gameState = GameFinished then raise (GameNotActiveException("Game has ended"))

    let validatePinCount pins = 
        if pins < 0 || pins > 10 then raise (InvalidPinCountException("Pin count should be higher than or equal to 0 and less than or equal to 10"))        
    


    let mutable frameScores  = Map.empty
    let mutable frameResults = Map.empty

    let mutable currentFrameNr = 1

    let mutable gameState = ReadyForFirstThrow

    let setFrameScore frameNr score =       
        frameScores.Add (frameNr,score)

    let setFrameResult frameNr frameResult =
        frameResults.Add (frameNr,frameResult)

    member this.CurrentScore = 
        frameScores
        |> Map.toSeq
        |> Seq.sumBy (fun (k,v) -> v)

    member this.Gooi pins =
        checkGameIsActive gameState
        validatePinCount pins        

        let frameScore = getFrameScore (currentFrameNr + pins) frameScores
        
        assertValidFrameScore frameScore

    
        if isRegularFrameNr currentFrameNr then
            assertValidFrameNr currentFrameNr
            frameScores <- setFrameScore currentFrameNr frameScore 

        for (frameNr,score) in updatePreviousFrameScores currentFrameNr pins frameResults frameScores gameState do
            frameScores <- setFrameScore frameNr score 


        gameState <- 
            match (frameScore, isFirstThrow gameState) with
            | (10, true) -> 
                frameResults <- setFrameResult currentFrameNr Strike
                ReadyForFirstThrow
            | (10, false) -> 
                frameResults <- setFrameResult currentFrameNr Spare
                ReadyForFirstThrow     
            | _ -> toggleFirstThrow gameState

        if isFirstThrow gameState then             
            currentFrameNr <- currentFrameNr + 1

        gameState <- checkIfGameFinished currentFrameNr gameState frameResults  

    member this.ScoreVoorFrame frameNr =
        [1..frameNr]        
        |> Seq.map (fun (i) -> getFrameScore i frameScores)        
        |> Seq.sum 

namespace BowlingGame
open System.Collections.Generic;
open System;

type InvalidPinCountException(message) =
   inherit Exception(message)

type InvalidFrameNumberExceptoin(message) =
   inherit Exception(message)

type GameNotActiveException(message) =
   inherit Exception(message)
   

type FrameResult = Normal | Spare | Strike

type GameState = ReadyForFirstThrow | ReadyForSecondThrow | GameFinished

type Game() =

    let frameScores = new Dictionary<int, int>()

    let frameResults = new Dictionary<int, FrameResult>()

    let mutable currentFrameNr = 1

    let mutable gameState = ReadyForFirstThrow

    let isValidFrameNr frameNr =
        frameNr >= 1 && frameNr <= 12 

    let isRegularFrameNr frameNr =
        frameNr >= 1 && frameNr <= 10

    let incrementFrameNr =
        currentFrameNr <- currentFrameNr + 1

    let isFirstThrow =
        gameState = ReadyForFirstThrow

    let toggleFirstThrow =
        match gameState with
            | ReadyForFirstThrow  -> gameState <- ReadyForSecondThrow
            | ReadyForSecondThrow -> gameState <- ReadyForFirstThrow
            | GameFinished        -> raise (GameNotActiveException("Game has ended"))

    let setFrameScore frameNr score =
        if not (isValidFrameNr frameNr) then raise (InvalidFrameNumberExceptoin("Frame number should be higher than or equal to 1 and less than or equal to 12"))     
        if isRegularFrameNr(frameNr) then
            if not (frameScores.ContainsKey(frameNr)) then frameScores.Add(frameNr, score)
            else frameScores.Item(frameNr) <- score

    let getFrameScore frameNr =
        if not(frameScores.ContainsKey(frameNr)) then frameScores.Add(frameNr, 0)
        frameScores.[frameNr];

    let setFrameResult frameNr frameResult =
        if not (isValidFrameNr frameNr) then raise (InvalidFrameNumberExceptoin("Frame number should be higher than or equal to 1 and less than or equal to 12"))     
        
        if not (frameResults.ContainsKey(frameNr)) then frameResults.Add(frameNr, frameResult)
        else frameResults.Item(frameNr) <- frameResult

    let getFrameResult frameNr =
        if not (frameResults.ContainsKey(frameNr)) then frameResults.Add(frameNr, Normal)
        frameResults.[frameNr];

    let updatePreviousFrameScores frameNr pins =        
        if frameNr > 1 then
            let previousFrameResult = getFrameResult(frameNr - 1)

            if (previousFrameResult = Spare && isFirstThrow) || previousFrameResult = Strike then
                setFrameScore (frameNr - 1) (getFrameScore(frameNr - 1) + pins)
            
            if frameNr > 2 then
                if getFrameResult(frameNr - 2) = Strike && previousFrameResult = Strike then
                    setFrameScore (frameNr - 2) (getFrameScore(frameNr - 2) + pins)    

    member this.CurrentScore = 
        Seq.sum(frameScores.Values)

    member this.Gooi pins =
        if gameState = GameFinished then raise (GameNotActiveException("Game has ended"))

        if pins < 0 || pins > 10 then raise (InvalidPinCountException("Pin count should be higher than or equal to 0 and less than or equal to 10"))        

        let frameScore = getFrameScore(currentFrameNr) + pins
        if frameScore > 10 then raise (InvalidPinCountException("Pin count is higher than number of currently remaining pins in frame"))

        let mutable frameResult = Normal

        setFrameScore currentFrameNr frameScore
        updatePreviousFrameScores currentFrameNr pins

        if frameScore = 10 then 
            if isFirstThrow then frameResult <- Strike
            else frameResult <- Spare
            gameState <- ReadyForFirstThrow
        else 
            toggleFirstThrow

        setFrameResult currentFrameNr frameResult

        if isFirstThrow then             
            incrementFrameNr
            if currentFrameNr > 12 ||
                (currentFrameNr = 12 && getFrameResult(11) = Normal) ||
                (currentFrameNr = 11 && getFrameResult(10) = Normal) then
                gameState <- GameFinished


    member this.ScoreVoorFrame frameNr =
        let mutable cumulativeScore = 0
        for i = 1 to frameNr do
            cumulativeScore <- cumulativeScore + getFrameScore(i)
        cumulativeScore

    member this.CurrentFrameNr =
        currentFrameNr
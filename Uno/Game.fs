module Game

[<Struct>]
type PlayerCount = PlayerCount of int

[<Struct>]
type PlayerId = PlayerId of int

[<Struct>]
type Direction = Direction of int

type Command = 
    | StartGame of StartGame
    | PlayCard of PlayCard

and StartGame = {
    Players: PlayerCount
    FirstCard: Card }

and PlayCard = {
    Player: PlayerId
    Card: Card
}

type Event = 
    | GameStarted of GameStarted
    | CardPlayed of CardPlayed

and GameStarted = {
    Players: PlayerCount
    FirstCard: Card
}
and CardPlayed = {
    NextPlayer: PlayerId
    Card: Card
}

type State = 
    | InitialState
    | GameInProgress of GameInProgress
and GameInProgress = {
    Players: PlayerCount
    NextPlayer: PlayerId
    TopCard: Card
    Direction: Direction
}    

type GameError = 
     | GameAlreadyStarted
     | TooFewPlayers
     | GameNotStarted

type Decide = Command -> State -> Result<Event list, GameError>
type Evolve = State -> Event -> State

// Step 1:
// Make the simplest implementation for the following signature
// Command -> State -> Event list Result

let nextPlayer = fun (PlayerId x) (PlayerCount players) (Direction direction) ->
    PlayerId ((x+direction+players) % players)

let decide : Decide = fun command state -> // failwith "Not Implemented"
    match (command, state) with  
        | (StartGame cmd, _) when cmd.Players = PlayerCount(1) ->  Result.Error TooFewPlayers
        | (StartGame cmd, GameInProgress _) ->  Result.Error GameAlreadyStarted
        | (StartGame cmd, _) ->  Result.Ok [ GameStarted { Players = cmd.Players; FirstCard = cmd.FirstCard  }  ]
        | (PlayCard cmd, GameInProgress state) ->  Result.Ok [ CardPlayed {
             NextPlayer = nextPlayer state.NextPlayer state.Players state.Direction;
             Card = cmd.Card 
             } ]
        | (PlayCard cmd, InitialState) -> Result.Error GameNotStarted 
        | _ ->  failwith "Not Implemented"

// Step 2:
// Make the simplest implementation for the following signature
// State -> Event list -> State
// s -> Event [] -> sbyte
 
//  (s + ([a;b;c] @ [e;f;g])) = (s + [a;b;c]) + [e; f; g]) 
// Important! Businesss logic must be in decision function (because it can change), evolution should do simple assignment
let evolve : Evolve =
    fun state event -> 
    match (state, event) with
    | (InitialState, GameStarted event) -> GameInProgress { Players=event.Players; NextPlayer=PlayerId(0); TopCard=event.FirstCard; Direction=Direction(+1)  }
    | (GameInProgress s, CardPlayed event) -> GameInProgress { s with TopCard = event.Card; NextPlayer = event.NextPlayer }
    | _ -> failwith "Not Implemented"   


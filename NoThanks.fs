open System

// Global variables
// Used to parameterise the game
let lowestCard = 3
let highestCard = 35
let numberOfDiscardedCards = 3

// GameState
//
// numberOfPlayers : the number of players in the game
// currentPlayer   : the index of the current player who must make a decision
// playerTokens    : list of the number of tokens each player holds
// playerCards     : list of lists of cards each player holds
// cardInPlay      : the card that is currently sat in the middle of the table waiting for a decision
// tokensInPlay    : the number of tokens on the card in the middle of the table
type GameState = {numberOfPlayers : int;
                  currentPlayer   : int;
                  playerTokens    : list<int>;
                  playerCards     : list<list<int>>;
                  cardInPlay      : option<int>;
                  tokensInPlay    : int}

// scoreFromCards
//
// The score from the cards you have
// The sum of the starting card of each 'run' of cards
let scoreFromCards (cards : list<int>) =
  let runStarts = cards |> List.filter (fun x -> (List.tryFind (fun y -> y = x-1) cards) = None)
  List.sum runStarts

// Player
//
// name : the name of the player
type Player = {name : string;
               strategy : GameState -> bool}
// printGameOutcome
//
// Prints the scores of the players and determines the winner
// gameState : the state of the game, normally passed in after play has ended (i.e. the deck is empty)
let rec printGameOutcome (gameState : GameState) (players : list<Player>) =
  match players with
  | [] -> ()
  | thisPlayer :: otherPlayers -> printfn "Player %A:" thisPlayer.name
                                  let cards = gameState.playerCards.Item 0
                                  let tokens = gameState.playerTokens.Item 0
                                  printfn "Cards: %A" (List.sort cards)
                                  printfn "Tokens: %A" tokens
                                  let score = (scoreFromCards cards) - tokens
                                  printfn "Score: %A" score
                                  let otherCards = match gameState.playerCards with
                                                   | a :: b -> b
                                                   | _ -> List.empty<list<int>>
                                  let otherTokens = match gameState.playerTokens with
                                                    | a :: b -> b
                                                    | _ -> List.empty<int>
                                  let reducedGameState = {gameState with playerTokens = otherTokens;
                                                                         playerCards = otherCards}
                                  printGameOutcome reducedGameState otherPlayers

// discardRandomCards
//
// Function to discard a number of cards at random from a deck
//
// randomNumberGenerator : random number generator used to choose which cards to discard
// deck                  : the deck from which we are to discard cards
// numberToDiscard       : the number of cards we will discard from the deck
let rec discardRandomCards (randomNumberGenerator : Random) numberToDiscard (deck : List<int>) =
  match numberToDiscard with
  | 0 -> deck
  | _ -> let discardIndex = randomNumberGenerator.Next(deck.Length)
         let newDeck = deck |> List.mapi (fun i x -> (i, x))
                            |> List.filter (fun x -> fst x <> discardIndex)
                            |> List.map (fun x -> snd x)
         discardRandomCards randomNumberGenerator (numberToDiscard - 1) newDeck


// playRoundOfNoThanks
//
// This function is passed data about the current state of the game
// randomNumberGenerator : used to choose which card to draw from the deck, also passed to players
let rec playRoundOfNoThanks (randomNumberGenerator : Random) (deck : list<int>) (gameState : GameState) (players : list<Player>) =
  let currentPlayer = players.Item gameState.currentPlayer
  let playerIndex = gameState.currentPlayer
  printfn "The current player is %A" currentPlayer.name

  match gameState.cardInPlay with
  | Some C -> let take = match gameState.playerTokens.Item playerIndex with
                         | 0 -> printfn "%A had to take the card since they had no tokens left." currentPlayer.name
                                true
                         | _ -> currentPlayer.strategy gameState
              match take with
              | true -> let newGameState = {numberOfPlayers = gameState.numberOfPlayers;
                                            currentPlayer = gameState.currentPlayer;
                                            playerTokens = gameState.playerTokens |> List.mapi (fun i x -> match i with
                                                                                                           | y when y = playerIndex -> x + gameState.tokensInPlay
                                                                                                           | _ -> x);
                                            playerCards = gameState.playerCards |> List.mapi (fun i x -> match i with
                                                                                                         | y when y = playerIndex -> C :: x
                                                                                                         | _ -> x);
                                            cardInPlay = None;
                                            tokensInPlay = 0}
                        printfn "%A chose to take the card." currentPlayer.name
                        playRoundOfNoThanks randomNumberGenerator deck newGameState players
              | false -> let newGameState = {gameState with currentPlayer = (playerIndex + 1)%(gameState.numberOfPlayers);
                                                            playerTokens = gameState.playerTokens |> List.mapi (fun i x -> match i with
                                                                                                                           | y when y = playerIndex -> x - 1
                                                                                                                           | _ -> x);
                                                            tokensInPlay = gameState.tokensInPlay + 1}
                         printfn "%A chose not to take the card." currentPlayer.name
                         playRoundOfNoThanks randomNumberGenerator deck newGameState players
  | None -> match deck with
            | [] -> printfn "The deck is empty and the game has finished"
                    printGameOutcome gameState players
                    ()
            | _ ->  let randNum = randomNumberGenerator.Next(deck.Length)
                    let C = deck.Item randNum
                    let newDeck = deck |> List.mapi (fun i x -> (i, x))
                                       |> List.filter (fun x -> fst x <> randNum)
                                       |> List.map (fun x -> snd x)
                    let newGameState = {gameState with cardInPlay = Some C}
                    printfn "The card %A was drawn, the deck is now: %A" C newDeck
                    playRoundOfNoThanks randomNumberGenerator newDeck newGameState players
  

// playGameOfNoThanks
//
// Function that will run a game of NoThanks and output a game log.
// 
// Arguments:
// seed    : the seed to the random number generator
// players : a list of player, i.e. strategies
//
// Output: FIXME - come up with an output format
let playGameOfNoThanks seed (players : List<Player>) =

  let randomNumberGenerator = Random(seed)

  let numberOfPlayers = players.Length

  // FIXME - can I really use None in this fashion?
  let currentGameState = {numberOfPlayers = numberOfPlayers;
                          currentPlayer = 0;
                          playerTokens = List.init numberOfPlayers (fun x -> 11);
                          playerCards = List.init numberOfPlayers (fun x -> List.empty<int>);
                          cardInPlay = None;
                          tokensInPlay = 0}

  // Initialise the deck, discarding the appropriate number of cards
  let deck = List.init (highestCard - lowestCard + 1) (fun x -> lowestCard + x)
              |> discardRandomCards randomNumberGenerator numberOfDiscardedCards

  // Print the initial state of the game
  printfn "The card deck at the start of the game is %A" deck

  // FIXME - start the first round
  playRoundOfNoThanks randomNumberGenerator deck currentGameState players

  // Return something (currently just doing this so that the thing actually compiles)
  ()
  
// Main code
playGameOfNoThanks 1234 [{name = "Harry"; strategy = fun x -> true};
                         {name = "JC"; strategy = fun x -> true}]

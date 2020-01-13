type player =
  | PlayerOne
  | PlayerTwo;


type point =
  | Love
  | Fifteen
  | Thirty
  | Forty;

type pointsData = {
  playerOne: point,
  playerTwo: point
};

type fortyData = {
  player: player, /* The player who have forty points */
  otherPlayerPoint: point
};

type score =
| Points(pointsData)
| Forty(fortyData)
| Deuce
| Advantage(player)
| Game(player);

let scoreWhenDeuce: player => score = winner => Advantage(winner);

let scoreWhenAdvantage: (player, player) => score =
  (advantagedPlayer, winner) =>
    advantagedPlayer == winner ? Game(winner) : Deuce;

let other = player =>
  switch player {
  | PlayerOne => PlayerTwo
  | PlayerTwo => PlayerOne
  };

/*let scoreWhenForty : ('a, player) => score =
  (current, winner) => 
  switch current.otherPlayerPoint {
  | Love => Forty({player: winner, otherPlayerPoint: Fifteen})
  | Fifteen => Forty({player: winner, otherPlayerPoint: Thirty})
  | Thirty => Deuce 
  | Forty =>  Game(winner)
  };*/

  /* We add a tool function to increment point */
let incrementPoint: point => option(point) =
point =>
  switch point {
  | Love => Some(Fifteen)
  | Fifteen => Some(Thirty)
  | Thirty => None
  | Forty => None
  };

let scoreWhenForty = (current, winner) =>
  current.player == winner ?
    Game(winner) :
    (
      switch (incrementPoint(current.otherPlayerPoint)) {
      | Some(p) => Forty({...current, otherPlayerPoint: p})
      | None => Deuce
      }
    );

let pointTo = (player, point, current) =>
  switch player {
  | PlayerOne => {...current, playerOne: point}
  | PlayerTwo => {...current, playerTwo: point}
  };

let pointFor = (player, current) =>
  switch player {
  | PlayerOne => current.playerOne
  | PlayerTwo => current.playerTwo
  };

let scoreWhenPoints = (current, winner) =>
  switch (current |> pointFor(winner) |> incrementPoint) {
  | Some(np) => Points(pointTo(winner, np, current))
  | None =>
    Forty({
      player: winner,
      otherPlayerPoint: current |> pointFor(other(winner))
    })
  };

let scoreWhenGame = winner => Game(winner);

let score = (current, winner) =>
  switch current {
  | Points(p) => scoreWhenPoints(p, winner)
  | Forty(f) => scoreWhenForty(f, winner)
  | Deuce => scoreWhenDeuce(winner)
  | Advantage(a) => scoreWhenAdvantage(a, winner)
  | Game(g) => scoreWhenGame(g)
  };

let newGame = Points({playerOne: Love, playerTwo: Love});

let string_of_player = (player) => 
  switch player {
  | PlayerOne => "Player One"
  | PlayerTwo => "Player Two"
  };

let string_of_point = (point) => 
  switch point {
  | Love => "0"
  | Fifteen => "15"
  | Thirty => "30"
  | Forty => "40"
  };

let string_of_score = (score) => 
  switch score {
  | Points(pointsData) => "Player One : " ++ string_of_point(pointsData.playerOne) ++ " points - Player Two : " ++ string_of_point(pointsData.playerTwo) ++ " points"
  | Forty(fortyData) => (fortyData.player == PlayerOne) ? string_of_player(fortyData.player) ++ " : 40 points - Player Two : " ++ string_of_point(fortyData.otherPlayerPoint) ++ " points" : "Player One : " ++ string_of_point(fortyData.otherPlayerPoint) ++ " points - " ++ string_of_player(fortyData.player) ++ " : 40 points"
  | Deuce => "This is a deuce between both players"
  | Advantage(player) => "Advantage to " ++ string_of_player(player)
  | Game(player) => string_of_player(player) ++ " won the game"
  };
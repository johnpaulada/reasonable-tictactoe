type mode =
  | PVP
  | PVC;

type player =
  | Circle
  | Cross;

type turn = option(player);

type cellValue =
  | Player(player)
  | Empty;

/* type board = (cellValue, cellValue, cellValue, cellValue, cellValue, cellValue, cellValue, cellValue, cellValue) */

type board = {
  topLeft: cellValue,
  topCenter: cellValue,
  topRight: cellValue,
  middleLeft: cellValue,
  middleCenter: cellValue,
  middleRight: cellValue,
  bottomLeft: cellValue,
  bottomCenter: cellValue,
  bottomRight: cellValue,
};

type status =
  | Playing
  | Win
  | Draw;

type gameState = {
  board,
  status,
  turn,
  mode,
};

type horizontalPosition =
  | Left
  | Center
  | Right;

type verticalPosition =
  | Top
  | Middle
  | Bottom;

type position = {
  horizontalPosition,
  verticalPosition,
};

type circlePosition =
  | CirclePosition(position);

type crossPosition =
  | CrossPosition(position);

type playerAction =
  | CircleMove(circlePosition)
  | CrossMove(crossPosition);

type move = (gameState, playerAction) => gameState;
type crossMoves = list(crossPosition);
type circleMoves = list(circlePosition);
type moveList =
  | CrossMoves(crossMoves)
  | CircleMoves(circleMoves);
type possibleCrossMoves = board => crossMoves;
type possibleCircleMoves = board => circleMoves;
type crossMovesToAction = crossMoves => playerAction;
type circleMovesToAction = circleMoves => playerAction;
type takeRandomCrossAction = crossMoves => option(playerAction);
type takeRandomCircleAction = circleMoves => playerAction;
type takeQCrossAction = crossMoves => playerAction;
type takeQCircleAction = circleMoves => playerAction;
type boardToHash = board => int;
type hashToBoard = int => board;
let doMove: move =
  (state, action) => {
    switch (action) {
    | CrossMove(
        CrossPosition({horizontalPosition: Left, verticalPosition: Top}),
      ) => {
        ...state,
        board: {
          ...state.board,
          topLeft: Player(Cross),
        },
        turn: Some(Circle),
      }
    | CrossMove(
        CrossPosition({horizontalPosition: Center, verticalPosition: Top}),
      ) => state
    | CrossMove(
        CrossPosition({horizontalPosition: Right, verticalPosition: Top}),
      ) => state
    | CrossMove(
        CrossPosition({horizontalPosition: Left, verticalPosition: Middle}),
      ) => state
    | CrossMove(
        CrossPosition({horizontalPosition: Center, verticalPosition: Middle}),
      ) => state
    | CrossMove(
        CrossPosition({horizontalPosition: Right, verticalPosition: Middle}),
      ) => state
    | CrossMove(
        CrossPosition({horizontalPosition: Left, verticalPosition: Bottom}),
      ) => state
    | CrossMove(
        CrossPosition({horizontalPosition: Center, verticalPosition: Bottom}),
      ) => state
    | CrossMove(
        CrossPosition({horizontalPosition: Right, verticalPosition: Bottom}),
      ) => state
    | CircleMove(
        CirclePosition({horizontalPosition: Left, verticalPosition: Top}),
      ) => state
    | CircleMove(
        CirclePosition({horizontalPosition: Center, verticalPosition: Top}),
      ) => state
    | CircleMove(
        CirclePosition({horizontalPosition: Right, verticalPosition: Top}),
      ) => state
    | CircleMove(
        CirclePosition({horizontalPosition: Left, verticalPosition: Middle}),
      ) => state
    | CircleMove(
        CirclePosition({
          horizontalPosition: Center,
          verticalPosition: Middle,
        }),
      ) => state
    | CircleMove(
        CirclePosition({horizontalPosition: Right, verticalPosition: Middle}),
      ) => state
    | CircleMove(
        CirclePosition({horizontalPosition: Left, verticalPosition: Bottom}),
      ) => state
    | CircleMove(
        CirclePosition({
          horizontalPosition: Center,
          verticalPosition: Bottom,
        }),
      ) => state
    | CircleMove(
        CirclePosition({horizontalPosition: Right, verticalPosition: Bottom}),
      ) => state
    };
  };

let boardToHash = board => {};

let initialize = (mode: mode, first: player) => {
  let board: board = {
    topLeft: Empty,
    topCenter: Empty,
    topRight: Empty,
    middleLeft: Empty,
    middleCenter: Empty,
    middleRight: Empty,
    bottomLeft: Empty,
    bottomCenter: Empty,
    bottomRight: Empty,
  };
  let state: gameState = {board, status: Playing, turn: Some(first), mode};
  state;
};

let state = initialize(PVP, Cross);
let action =
  CrossMove(
    CrossPosition({horizontalPosition: Left, verticalPosition: Top}),
  );

doMove(state, action);

/*
 until play_count:
   initialize game
   while playing:
     check whos turn
     make move
     check if win
 */

let leftTopCrossMove = (board, moves) => {
  switch (board) {
  | {topLeft: Empty} => [
      CrossPosition({horizontalPosition: Left, verticalPosition: Top}),
      ...moves,
    ]
  | _ => moves
  };
};

let centerTopCrossMove = (board, moves) => {
  switch (board) {
    | {topCenter: Empty} => [
        CrossPosition({horizontalPosition: Center, verticalPosition: Top}),
        ...moves,
        ]
    | _ => moves
  };
};

let rightTopCrossMove = (board, moves) => {
  switch (board) {
    | {topRight: Empty} => [
        CrossPosition({horizontalPosition: Right, verticalPosition: Top}),
        ...moves,
        ]
    | _ => moves
  };
};

let possibleCrossMoves: possibleCrossMoves =
  board => {
    []
    |> leftTopCrossMove(board)
    |> centerTopCrossMove(board)
    |> rightTopCrossMove(board);
  };

possibleCrossMoves(state.board);

let takeRandomCrossAction: takeRandomCrossAction =
  crossMoves => {
    let shuffledPosition = crossMoves->Belt.List.shuffle->Belt.List.head;

    switch (shuffledPosition) {
        | Some(position) => Some(CrossMove(position))
        | None => None
    };
  };

Js.log(takeRandomCrossAction(possibleCrossMoves(state.board)));
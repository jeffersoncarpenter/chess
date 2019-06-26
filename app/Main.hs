{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import qualified Chess as CH
import Chess (PieceType(..), Color(..))
import Chess.PGN
import Control.Monad
import Data.List
import Data.Maybe
import Lib

type Square = (Int, Int)

other_color :: Color -> Color
other_color White = Black
other_color Black = White

type Piece = PieceType

data GamePiece = GamePiece
  { square :: Square
  , piece :: Piece
  , color :: Color } deriving (Show, Eq)

data Chess = Chess
  Color
  [GamePiece]


class HasMaterial a where
  material :: a -> Double

instance HasMaterial PieceType where
  material Pawn = 1
  material Rook = 5
  material Knight = 3
  material Bishop = 3
  material Queen = 9
  material King = 0

color_coef :: Color -> Double
color_coef White = 1
color_coef Black = -1

pawn_direction :: Color -> Int
pawn_direction White = 1
pawn_direction Black = -1

pawn_double_move :: Color -> Int
pawn_double_move White = 1
pawn_double_move Black = 6

instance HasMaterial GamePiece where
  material x = (material . piece) x

instance HasMaterial a => HasMaterial [a] where
  material = foldl' (+) 1 . fmap material

square_coefficient :: Square -> Double
square_coefficient (x, y) = centrality x * centrality y
  where
    centrality x = case x of
      0 -> 1
      1 -> 1.1
      2 -> 1.3
      3 -> 1.6
      4 -> 1.6
      5 -> 1.3
      6 -> 1.1
      7 -> 1

legal_position :: Square -> Bool
legal_position (x, y) = is_board_position x && is_board_position y
  where
    is_board_position x = x >= 0 && x <= 7

add_square :: Square -> Square -> Square
add_square (x, y) (x2, y2) = (x + x2, y + y2)

distances :: [Int]
distances = [-7 .. 7]

data Move = Move
  { game_piece :: GamePiece
  , destination :: Square
  , capture :: Maybe GamePiece } deriving Show

occupies_square :: Square -> GamePiece -> Maybe GamePiece
occupies_square s p = if (square p) == s
                      then Just p
                      else Nothing

occupied_square :: Square -> [GamePiece] -> Maybe GamePiece
occupied_square s = msum . fmap (occupies_square s)

try_move :: [GamePiece] -> GamePiece -> Square -> Maybe Move
try_move ps p s =
  let
    dest = add_square s (square p)
  in
    if legal_position dest
    then
      case occupied_square dest ps of
        Nothing -> Just $ Move p dest Nothing
        Just p2 ->
          if (color p) == (color p2)
          then Nothing
          else Just $ Move p dest (Just p2)
    else Nothing

take_until_nothing :: [Maybe Move] -> [Move]
take_until_nothing xs = helper [] xs
  where helper ms [] = ms
        helper ms (Nothing:xs) = ms
        helper ms ((Just m):xs) = case m of
          Move _ _ Nothing -> helper (m:ms) xs
          _ -> (m:ms)

enough :: [Int]
enough = [1..7]

up_shifts :: [Square]
up_shifts = fmap (\x -> (0, x)) enough

down_shifts :: [Square]
down_shifts = fmap (\x -> (0, -x)) enough

left_shifts :: [Square]
left_shifts = fmap (\x -> (-x, 0)) enough

right_shifts :: [Square]
right_shifts = fmap (\x -> (x, 0)) enough

upleft_shifts :: [Square]
upleft_shifts = fmap (\x -> (-x, x)) enough

upright_shifts :: [Square]
upright_shifts = fmap (\x -> (x, x)) enough

downleft_shifts :: [Square]
downleft_shifts = fmap (\x -> (-x, -x)) enough

downright_shifts :: [Square]
downright_shifts = fmap (\x -> (x, -x)) enough

legal_moves :: [GamePiece] -> GamePiece -> [Move]
legal_moves ps p =
  let (x, y) = square p
      f = try_move ps p in
    case p of
      (GamePiece s Pawn c) ->
        let
          capture_only x = case x of
            Just (Move _ _ Nothing) -> Nothing
            a -> a
          move_only x = case x of
            Just (Move _ _ (Just _)) -> Nothing
            a -> a
          move_up = move_only $ f (0, (pawn_direction c))
          move_double_up = move_only $
            if y == pawn_double_move c
            then f (0, 2 * pawn_direction c)
            else Nothing
          capture_left = capture_only $ f (1, (pawn_direction c))
          capture_right = capture_only $ f (-1, (pawn_direction c))
        in
          catMaybes
          [ capture_left
          , capture_right ] ++
          ( take_until_nothing $ [ move_up
                                 , move_double_up ] )
      (GamePiece s Rook c) ->
        (take_until_nothing $ fmap f up_shifts) ++
        (take_until_nothing $ fmap f down_shifts) ++
        (take_until_nothing $ fmap f left_shifts) ++
        (take_until_nothing $ fmap f right_shifts)
      (GamePiece s Knight c) ->
        catMaybes $ fmap f
        [ (1, 2)
        , (1, -2)
        , (-1, 2)
        , (-1, -2)
        , (2, 1)
        , (2, -1)
        , (-2, 1)
        , (-2, -1) ]
      (GamePiece s Bishop c) ->
        (take_until_nothing $ fmap f upleft_shifts) ++
        (take_until_nothing $ fmap f upright_shifts) ++
        (take_until_nothing $ fmap f downleft_shifts) ++
        (take_until_nothing $ fmap f downright_shifts)
      (GamePiece s Queen c) ->
        (take_until_nothing $ fmap f up_shifts) ++
        (take_until_nothing $ fmap f down_shifts) ++
        (take_until_nothing $ fmap f left_shifts) ++
        (take_until_nothing $ fmap f right_shifts) ++
        (take_until_nothing $ fmap f upleft_shifts) ++
        (take_until_nothing $ fmap f upright_shifts) ++
        (take_until_nothing $ fmap f downleft_shifts) ++
        (take_until_nothing $ fmap f downright_shifts)
      (GamePiece s King c) ->
        catMaybes $ fmap f
        [ (1, -1)
        , (1, 0)
        , (1, 1)
        , (0, -1)
        , (0, 1)
        , (-1, -1)
        , (-1, 0)
        , (-1, 1) ]

instance HasMaterial Move where
  material (Move _ _ Nothing) = 0
  material (Move _ _ (Just p)) = material p


commit_move :: [GamePiece] -> Move -> [GamePiece]
commit_move [] m = []
commit_move (p:ps) m =
  let (Move p' d _) = m in
    if p == p'
    then p {square = d} : ps
    else p : commit_move ps m


score_move :: [GamePiece] -> [Move] -> Move -> Double
score_move ps ms m =
  let
    Move p dest capture = m

    c' = other_color (color p)
    p' = p { color = c' }

    next_moves = legal_moves ps p {square = dest}

    moves_as_if_enemy = legal_moves ps p'
    next_moves_as_if_enemy = legal_moves ps p' {square = dest}

    material_gained = material m

    threats_left = foldl' (+) 0 $ fmap material ms
    threats_gained = foldl' (+) 0 $ fmap material next_moves

    backup_left = foldl' (+) 0 $ fmap material moves_as_if_enemy
    backup_gained = foldl' (+) 0 $ fmap material next_moves_as_if_enemy

    influence_left = (foldl' (+) 0 $ fmap (square_coefficient . destination) ms)
    influence_gained = (foldl' (+) 0 $ fmap (square_coefficient . destination) next_moves)

    pawn_influence = case p of
      GamePiece _ Pawn _ ->
        1.5 +
        if (length ms == 2)
        then (square_coefficient . destination) m
        else 0
      _ -> 0
  in
    -- pawn_influence
    material_gained + threats_gained - threats_left + influence_gained - influence_left + pawn_influence

all_moves :: Color -> [GamePiece] -> [Move]
all_moves c ps =
  let
    each_piece_moves = fmap (legal_moves ps) $ [x | x <- ps, color x == c]
  in
    concat each_piece_moves

king_in_danger :: Color -> [GamePiece] -> Bool
king_in_danger c ps =
  let
    ms = all_moves (other_color c) ps
    attacks_king cap = case cap of
      Just (GamePiece _ King c) -> True
      _ -> False
  in
    foldl' (||) False (fmap (attacks_king . capture) ms)

initial_position :: [GamePiece]
initial_position =
  [ GamePiece (0, 0) Rook White
  , GamePiece (1, 0) Knight White
  , GamePiece (2, 0) Bishop White
  , GamePiece (3, 0) Queen White
  , GamePiece (4, 0) King White
  , GamePiece (5, 0) Bishop White
  , GamePiece (6, 0) Knight White
  , GamePiece (7, 0) Rook White
  , GamePiece (0, 1) Pawn White
  , GamePiece (1, 1) Pawn White
  , GamePiece (2, 1) Pawn White
  , GamePiece (3, 1) Pawn White
  , GamePiece (4, 1) Pawn White
  , GamePiece (5, 1) Pawn White
  , GamePiece (6, 1) Pawn White
  , GamePiece (7, 1) Pawn White ]
   ++
  [ GamePiece (0, 7) Rook Black
  , GamePiece (1, 7) Knight Black
  , GamePiece (2, 7) Bishop Black
  , GamePiece (3, 7) Queen Black
  , GamePiece (4, 7) King Black
  , GamePiece (5, 7) Bishop Black
  , GamePiece (6, 7) Knight Black
  , GamePiece (7, 7) Rook Black
  , GamePiece (0, 6) Pawn Black
  , GamePiece (1, 6) Pawn Black
  , GamePiece (2, 6) Pawn Black
  , GamePiece (3, 6) Pawn Black
  , GamePiece (4, 6) Pawn Black
  , GamePiece (5, 6) Pawn Black
  , GamePiece (6, 6) Pawn Black
  , GamePiece (7, 6) Pawn Black ]

test_score :: [Double]
test_score =
  [ let piece = (GamePiece (4, 1) Pawn White)
        lms = legal_moves initial_position piece
    in
        score_move initial_position lms (lms !! 0)
  , let piece = (GamePiece (2, 1) Pawn White)
        lms = legal_moves initial_position piece
    in
        score_move initial_position lms (lms !! 0)
  ]

sort_moves c ps =
  let
    each_piece_moves = fmap (legal_moves ps) $ [x | x <- ps, color x == c]
    scored_moves = fmap (\moves -> (fmap (\move -> (move, score_move ps moves move))) moves) each_piece_moves
  in
  sortBy (\(x, y) (x', y') -> compare y' y) (concat scored_moves)

ordered_moves :: Color -> [GamePiece] -> [Move]
ordered_moves c ps = fmap fst (sort_moves c ps)


type Test = (String, Bool)

tests :: [Test]
tests =
  []

runTest :: Test -> Maybe String
runTest (x, a) = if a then Just x else Nothing

run_tests :: [Test] -> String
run_tests ts = case msum $ fmap runTest ts of
  Just x -> "Test failed: " ++ x
  Nothing -> "All tests passed."


class FromCH a b where
  fromCH :: a -> b

instance FromCH CH.Color Color where
  fromCH CH.White = White
  fromCH CH.Black = Black

coord_to_digit :: Int -> Char
coord_to_digit 0 = '1'
coord_to_digit 1 = '2'
coord_to_digit 2 = '3'
coord_to_digit 3 = '4'
coord_to_digit 4 = '5'
coord_to_digit 5 = '6'
coord_to_digit 6 = '7'
coord_to_digit 7 = '8'

coord_to_char :: Int -> Char
coord_to_char 0 = 'a'
coord_to_char 1 = 'b'
coord_to_char 2 = 'c'
coord_to_char 3 = 'd'
coord_to_char 4 = 'e'
coord_to_char 5 = 'f'
coord_to_char 6 = 'g'
coord_to_char 7 = 'h'

digit_to_coord :: Char -> Int
digit_to_coord '1' = 0
digit_to_coord '2' = 1
digit_to_coord '3' = 2
digit_to_coord '4' = 3
digit_to_coord '5' = 4
digit_to_coord '6' = 5
digit_to_coord '7' = 6
digit_to_coord '8' = 7

char_to_coord :: Char -> Int
char_to_coord 'a' = 0
char_to_coord 'b' = 1
char_to_coord 'c' = 2
char_to_coord 'd' = 3
char_to_coord 'e' = 4
char_to_coord 'f' = 5
char_to_coord 'g' = 6
char_to_coord 'h' = 7

square_to_coords :: Square -> String
square_to_coords (x, y) = coord_to_char x : coord_to_digit y : []

coords_to_square :: String -> Square
coords_to_square (x:y:[]) = (char_to_coord x, digit_to_coord y)

get_game_piece :: CH.GameState -> Square -> Maybe GamePiece
get_game_piece gs s =
  let
    maybe_piece = CH.pieceAt (CH.board gs) (square_to_coords s)
  in
    case maybe_piece of
      Nothing -> Nothing
      Just (CH.Piece c p) -> Just $ GamePiece s p c

all_squares :: [Square]
all_squares = pure (\x y -> (x, y)) <*> [0..7] <*> [0..7]

instance FromCH CH.GameState Chess where
  fromCH gs = Chess
    (CH.currentPlayer gs)
    (catMaybes $ fmap (get_game_piece gs) all_squares)


toChessMove :: Move -> String
toChessMove (Move (GamePiece src _ _) dst c) =
  case c of
    Nothing -> square_to_coords src ++ "-" ++ square_to_coords dst
    _ -> square_to_coords src ++ "-" ++ square_to_coords dst

verify_move :: Color -> [GamePiece] -> Move -> Bool
verify_move c ps m =
  let
    ps' = commit_move ps m
  in
    if king_in_danger c ps'
    then False
    else True

makeMove :: IO ()
makeMove = do
  fen_str <- getLine
  let fen = readPGN fen_str
  case fen of
    Just gs -> do
      let Chess c ps = fromCH gs
      -- putStrLn $ show (score_moves c ps)
      let ms = [m | m <- ordered_moves c ps, verify_move c ps m]
      -- putStrLn (toChessMove (ms !! 0))
      -- putStrLn (show (ms !! 0))
      -- putStrLn (show ps)
      let n =
            if 0 == length ms
            then putStrLn "No moves!"
            else do
              let gs'_ = CH.move gs (toChessMove (ms !! 0))
              case gs'_ of
                Nothing -> do
                  putStrLn (toChessMove (ms !! 0))
                  putStrLn (show (ms !! 0))
                  putStrLn (show ps)
                  putStrLn "Generated move did not work"
                Just gs' -> do
                  putStrLn $ writeFEN gs'
      n
    Nothing -> putStrLn "Could not parse FEN"

main :: IO ()
main = do
  -- putStrLn $ run_tests tests
  -- putStrLn $ show $ ordered_moves White initial_position
  -- let piece = (GamePiece (4, 1) Pawn White)
  -- let lms = legal_moves initial_position piece
  -- putStrLn (show lms)
  -- sequence_ $ fmap (putStrLn . show) test_score
  makeMove

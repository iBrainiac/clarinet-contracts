
(define-constant THIS_CONTRACT (as-contract tx-sender)) ;; The address of this contract itself
(define-constant ERR_MIN_BET_AMOUNT u100) ;; Error thrown when a player tries to create a game with a bet amount less than the minimum (0.0001 STX)
(define-constant ERR_INVALID_MOVE u101) ;; Error thrown when a move is invalid, i.e. not within range of the board or not an X or an O
(define-constant ERR_GAME_NOT_FOUND u102) ;; Error thrown when a game cannot be found given a Game ID, i.e. invalid Game ID
(define-constant ERR_GAME_CANNOT_BE_JOINED u103) ;; Error thrown when a game cannot be joined, usually because it already has two players
(define-constant ERR_NOT_YOUR_TURN u104) ;; Error thrown when a player tries to make a move when it is not their turn

;; The Game ID to use for the next game - start at 1 so first game gets ID 0
(define-data-var latest-game-id uint u1)

;; Player statistics for leaderboard
(define-map player-stats 
    principal ;; Key (Player Address)
    { ;; Value (Player Stats)
        games-played: uint,
        games-won: uint,
        games-lost: uint,
        games-drawn: uint,
        total-winnings: uint,
        total-losses: uint,
        current-win-streak: uint,
        best-win-streak: uint,
        join-timestamp: uint
    }
)

;; List to track all players who have played (for leaderboard iteration)
(define-data-var registered-players (list 1000 principal) (list))

(define-map games 
    uint ;; Key (Game ID)
    { ;; Value (Game Tuple)
        player-one: principal,
        player-two: (optional principal),
        is-player-one-turn: bool,

        bet-amount: uint,
        board: (list 9 uint),
        is-game-over: bool,
        winner: (optional principal)
    }
)

(define-public (create-game (bet-amount uint) (move-index uint) (move uint))
    (let (
        ;; Get the Game ID to use for creation of this new game (current value before increment)
        (game-id (- (var-get latest-game-id) u1))
        ;; The initial starting board for the game with all cells empty
        (starting-board (list u0 u0 u0 u0 u0 u0 u0 u0 u0))
        ;; Updated board with the starting move played by the game creator (X)
        (game-board (unwrap! (replace-at? starting-board move-index move) (err ERR_INVALID_MOVE)))
        ;; Create the game data tuple (player one address, bet amount, game board, and mark next turn to be player two's turn to join)
        (game-data {
            player-one: contract-caller,
            player-two: none,
            is-player-one-turn: false, ;; After creation, we wait for player two to join
            bet-amount: bet-amount,
            board: game-board,
            is-game-over: false,
            winner: none
        })
    )

    ;; Ensure that user has put up a bet amount greater than 0
    (asserts! (> bet-amount u0) (err ERR_MIN_BET_AMOUNT))
    ;; Ensure that the move being played is an `X`, not an `O`
    (asserts! (is-eq move u1) (err ERR_INVALID_MOVE))
    ;; Ensure that the move meets validity requirements
    (asserts! (validate-move starting-board move-index move) (err ERR_INVALID_MOVE))

    ;; Transfer the bet amount STX from user to this contract
    (try! (stx-transfer? bet-amount contract-caller THIS_CONTRACT))
    ;; Register player if not already registered
    (try! (register-player contract-caller))
    ;; Update the games map with the new game data
    (map-set games game-id game-data)
    ;; Increment the Game ID counter for next game
    (var-set latest-game-id (+ (var-get latest-game-id) u1))

    ;; Log the creation of the new game
    (print { action: "create-game", data: game-data})
    ;; Return the Game ID of the new game
    (ok game-id)
))

(define-public (join-game (game-id uint) (move-index uint) (move uint))
    (let (
        ;; Load the game data for the game being joined, throw an error if Game ID is invalid
        (original-game-data (unwrap! (map-get? games game-id) (err ERR_GAME_NOT_FOUND)))
        ;; Get the original board from the game data
        (original-board (get board original-game-data))

        ;; Update the game board by placing the player's move at the specified index
        (game-board (unwrap! (replace-at? original-board move-index move) (err ERR_INVALID_MOVE)))
        ;; Update the copy of the game data with the updated board and marking the next turn to be player one's turn
        (game-data (merge original-game-data {
            board: game-board,
            player-two: (some contract-caller),
            is-player-one-turn: true ;; After player two joins, it's player one's turn to play
        }))
    )

    ;; Ensure that the game being joined is able to be joined
    ;; i.e. player-two is currently empty
    (asserts! (is-none (get player-two original-game-data)) (err ERR_GAME_CANNOT_BE_JOINED)) 
    ;; Ensure that the move being played is an `O`, not an `X`
    (asserts! (is-eq move u2) (err ERR_INVALID_MOVE))
    ;; Ensure that the move meets validity requirements
    (asserts! (validate-move original-board move-index move) (err ERR_INVALID_MOVE))

    ;; Transfer the bet amount STX from user to this contract
    (try! (stx-transfer? (get bet-amount original-game-data) contract-caller THIS_CONTRACT))
    ;; Register player if not already registered
    (try! (register-player contract-caller))
    ;; Update the games map with the new game data
    (map-set games game-id game-data)

    ;; Log the joining of the game
    (print { action: "join-game", data: game-data})
    ;; Return the Game ID of the game
    (ok game-id)
))

(define-public (play (game-id uint) (move-index uint) (move uint))
    (let (
        ;; Load the game data for the game being joined, throw an error if Game ID is invalid
        (original-game-data (unwrap! (map-get? games game-id) (err ERR_GAME_NOT_FOUND)))
        ;; Get the original board from the game data
        (original-board (get board original-game-data))

        ;; Is it player one's turn?
        (is-player-one-turn (get is-player-one-turn original-game-data))
        ;; Get the player whose turn it currently is based on the is-player-one-turn flag
        (player-turn (if is-player-one-turn (get player-one original-game-data) (unwrap! (get player-two original-game-data) (err ERR_GAME_NOT_FOUND))))
        ;; Get the expected move based on whose turn it is (X or O?)
        (expected-move (if is-player-one-turn u1 u2))

        ;; Update the game board by placing the player's move at the specified index
        (game-board (unwrap! (replace-at? original-board move-index move) (err ERR_INVALID_MOVE)))
        ;; Check if the game has been won now with this modified board
        (is-now-winner (has-won game-board))
        ;; Check if the game is a draw (board full with no winner)
        (is-draw (and (not is-now-winner) (is-board-full game-board)))
        ;; Check if game is over (win or draw)
        (is-game-over (or is-now-winner is-draw))
        ;; Merge the game data with the updated board and toggle turn
        ;; Also mark the winner if the game has been won
        (game-data (merge original-game-data {
            board: game-board,
            is-player-one-turn: (not is-player-one-turn),
            is-game-over: is-game-over,
            winner: (if is-now-winner (some player-turn) none)
        }))
    )

    ;; Ensure that the function is being called by the player whose turn it is
    (asserts! (is-eq player-turn contract-caller) (err ERR_NOT_YOUR_TURN))
    ;; Ensure that the move being played is the correct move based on the current turn (X or O)
    (asserts! (is-eq move expected-move) (err ERR_INVALID_MOVE))
    ;; Ensure that the move meets validity requirements
    (asserts! (validate-move original-board move-index move) (err ERR_INVALID_MOVE))

    ;; Handle game completion (win or draw)
    (if is-game-over
        (begin
            ;; Handle payouts
            (if is-now-winner
                ;; Winner takes all (both bets)
                (try! (as-contract (stx-transfer? (* u2 (get bet-amount game-data)) tx-sender player-turn)))
                ;; Draw: return original bets to both players
                (begin
                    (try! (as-contract (stx-transfer? (get bet-amount game-data) tx-sender (get player-one game-data))))
                    (try! (as-contract (stx-transfer? (get bet-amount game-data) tx-sender (unwrap! (get player-two game-data) (err ERR_GAME_NOT_FOUND)))))
                )
            )
            ;; Update player statistics
            (try! (update-player-stats-on-game-end 
                (get player-one game-data) 
                (unwrap! (get player-two game-data) (err ERR_GAME_NOT_FOUND))
                (get winner game-data)
                (get bet-amount game-data)
            ))
        )
        false
    )

    ;; Update the games map with the new game data
    (map-set games game-id game-data)

    ;; Log the action of a move being made
    (print {action: "play", data: game-data})
    ;; Return the Game ID of the game
    (ok game-id)
))

(define-read-only (get-game (game-id uint))
    (map-get? games game-id)
)

(define-read-only (get-latest-game-id)
    (var-get latest-game-id)
)


;; ======================== LEADRBOARD IMPLEMENTATION ======================================

;; Get player statistics
(define-read-only (get-player-stats (player principal))
    (map-get? player-stats player)
)

;; Get all registered players
(define-read-only (get-registered-players)
    (var-get registered-players)
)

;; Get leaderboard by wins (simplified - returns first 10 registered players with their wins)
(define-read-only (get-leaderboard-by-wins)
    (let (
        (players (var-get registered-players))
        (first-ten (default-to (list) (slice? players u0 u10)))
    )
    (map get-player-wins-entry first-ten)
    )
)

;; Get leaderboard by win rate (simplified - returns first 10 registered players with their win rate)
(define-read-only (get-leaderboard-by-win-rate)
    (let (
        (players (var-get registered-players))
        (first-ten (default-to (list) (slice? players u0 u10)))
    )
    (map get-player-win-rate-entry first-ten)
    )
)

;; Get leaderboard by total winnings (simplified - returns first 10 registered players with their winnings)
(define-read-only (get-leaderboard-by-winnings)
    (let (
        (players (var-get registered-players))
        (first-ten (default-to (list) (slice? players u0 u10)))
    )
    (map get-player-winnings-entry first-ten)
    )
)

;; Get leaderboard by current win streak (simplified - returns first 10 registered players with their streak)
(define-read-only (get-leaderboard-by-streak)
    (let (
        (players (var-get registered-players))
        (first-ten (default-to (list) (slice? players u0 u10)))
    )
    (map get-player-streak-entry first-ten)
    )
)

;; Register a new player or update existing player's join timestamp
(define-private (register-player (player principal))
    (let (
        (existing-stats (map-get? player-stats player))
        (current-players (var-get registered-players))
    )
    (if (is-none existing-stats)
        (begin
            ;; New player - create initial stats
            (map-set player-stats player {
                games-played: u0,
                games-won: u0,
                games-lost: u0,
                games-drawn: u0,
                total-winnings: u0,
                total-losses: u0,
                current-win-streak: u0,
                best-win-streak: u0,
                join-timestamp: stacks-block-height
            })
            ;; Add to registered players list if not already there
            (if (is-none (index-of? current-players player))
                (var-set registered-players (unwrap! (as-max-len? (append current-players player) u1000) (err u999)))
                true
            )
            (ok true)
        )
        (ok true) ;; Player already exists
    ))
)

;; Update player statistics when a game ends
(define-private (update-player-stats-on-game-end (player-one principal) (player-two principal) (winner (optional principal)) (bet-amount uint))
    (let (
        (p1-stats (unwrap! (map-get? player-stats player-one) (err u999)))
        (p2-stats (unwrap! (map-get? player-stats player-two) (err u999)))
    )
    (if (is-some winner)
        ;; Someone won
        (let (
            (winner-addr (unwrap! winner (err u999)))
            (is-p1-winner (is-eq winner-addr player-one))
        )
        (if is-p1-winner
            ;; Player 1 won
            (begin
                (map-set player-stats player-one (merge p1-stats {
                    games-played: (+ (get games-played p1-stats) u1),
                    games-won: (+ (get games-won p1-stats) u1),
                    total-winnings: (+ (get total-winnings p1-stats) (* u2 bet-amount)),
                    current-win-streak: (+ (get current-win-streak p1-stats) u1),
                    best-win-streak: (if (> (+ (get current-win-streak p1-stats) u1) (get best-win-streak p1-stats))
                                        (+ (get current-win-streak p1-stats) u1)
                                        (get best-win-streak p1-stats))
                }))
                (map-set player-stats player-two (merge p2-stats {
                    games-played: (+ (get games-played p2-stats) u1),
                    games-lost: (+ (get games-lost p2-stats) u1),
                    total-losses: (+ (get total-losses p2-stats) bet-amount),
                    current-win-streak: u0
                }))
            )
            ;; Player 2 won
            (begin
                (map-set player-stats player-two (merge p2-stats {
                    games-played: (+ (get games-played p2-stats) u1),
                    games-won: (+ (get games-won p2-stats) u1),
                    total-winnings: (+ (get total-winnings p2-stats) (* u2 bet-amount)),
                    current-win-streak: (+ (get current-win-streak p2-stats) u1),
                    best-win-streak: (if (> (+ (get current-win-streak p2-stats) u1) (get best-win-streak p2-stats))
                                        (+ (get current-win-streak p2-stats) u1)
                                        (get best-win-streak p2-stats))
                }))
                (map-set player-stats player-one (merge p1-stats {
                    games-played: (+ (get games-played p1-stats) u1),
                    games-lost: (+ (get games-lost p1-stats) u1),
                    total-losses: (+ (get total-losses p1-stats) bet-amount),
                    current-win-streak: u0
                }))
            )
        ))
        ;; Draw
        (begin
            (map-set player-stats player-one (merge p1-stats {
                games-played: (+ (get games-played p1-stats) u1),
                games-drawn: (+ (get games-drawn p1-stats) u1)
            }))
            (map-set player-stats player-two (merge p2-stats {
                games-played: (+ (get games-played p2-stats) u1),
                games-drawn: (+ (get games-drawn p2-stats) u1)
            }))
        )
    )
    (ok true))
)

;; Check if board is full (for draw detection)
(define-private (is-board-full (board (list 9 uint)))
    (is-eq (len (filter is-cell-empty board)) u0)
)

;; Helper function to check if a cell is empty
(define-private (is-cell-empty (cell uint))
    (is-eq cell u0)
)

;; Helper functions for leaderboard entries
(define-private (get-player-wins-entry (player principal))
    (let (
        (stats (default-to 
            {games-played: u0, games-won: u0, games-lost: u0, games-drawn: u0, total-winnings: u0, total-losses: u0, current-win-streak: u0, best-win-streak: u0, join-timestamp: u0}
            (map-get? player-stats player)
        ))
    )
    {player: player, wins: (get games-won stats)}
    )
)

(define-private (get-player-win-rate-entry (player principal))
    (let (
        (stats (default-to 
            {games-played: u0, games-won: u0, games-lost: u0, games-drawn: u0, total-winnings: u0, total-losses: u0, current-win-streak: u0, best-win-streak: u0, join-timestamp: u0}
            (map-get? player-stats player)
        ))
        (games-played (get games-played stats))
        (games-won (get games-won stats))
        (win-rate (if (> games-played u0) (/ (* games-won u100) games-played) u0))
    )
    {player: player, win-rate: win-rate}
    )
)

(define-private (get-player-winnings-entry (player principal))
    (let (
        (stats (default-to 
            {games-played: u0, games-won: u0, games-lost: u0, games-drawn: u0, total-winnings: u0, total-losses: u0, current-win-streak: u0, best-win-streak: u0, join-timestamp: u0}
            (map-get? player-stats player)
        ))
    )
    {player: player, winnings: (get total-winnings stats)}
    )
)

(define-private (get-player-streak-entry (player principal))
    (let (
        (stats (default-to 
            {games-played: u0, games-won: u0, games-lost: u0, games-drawn: u0, total-winnings: u0, total-losses: u0, current-win-streak: u0, best-win-streak: u0, join-timestamp: u0}
            (map-get? player-stats player)
        ))
    )
    {player: player, streak: (get current-win-streak stats)}
    )
)

(define-private (validate-move (board (list 9 uint)) (move-index uint) (move uint))
    (let (
        ;; Validate that the move is being played within range of the board
        (index-in-range (and (>= move-index u0) (< move-index u9)))

        ;; Validate that the move is either an X or an O
        (x-or-o (or (is-eq move u1) (is-eq move u2)))

        ;; Validate that the cell the move is being played on is currently empty
        (empty-spot (is-eq (unwrap! (element-at? board move-index) false) u0))
    )

    ;; All three conditions must be true for the move to be valid
    (and (is-eq index-in-range true) (is-eq x-or-o true) empty-spot)
))

;; Given a board, return true if any possible three-in-a-row line has been completed
(define-private (has-won (board (list 9 uint))) 
    (or
        (is-line board u0 u1 u2) ;; Row 1
        (is-line board u3 u4 u5) ;; Row 2
        (is-line board u6 u7 u8) ;; Row 3
        (is-line board u0 u3 u6) ;; Column 1
        (is-line board u1 u4 u7) ;; Column 2
        (is-line board u2 u5 u8) ;; Column 3
        (is-line board u0 u4 u8) ;; Left to Right Diagonal
        (is-line board u2 u4 u6) ;; Right to Left Diagonal
    )
)

;; Given a board and three cells to look at on the board
;; Return true if all three are not empty and are the same value (all X or all O)
;; Return false if any of the three is empty or a different value
(define-private (is-line (board (list 9 uint)) (a uint) (b uint) (c uint)) 
    (let (
        ;; Value of cell at index a
        (a-val (unwrap! (element-at? board a) false))
        ;; Value of cell at index b
        (b-val (unwrap! (element-at? board b) false))
        ;; Value of cell at index c
        (c-val (unwrap! (element-at? board c) false))
    )

    ;; a-val must equal b-val and must also equal c-val while not being empty (non-zero)
    (and (is-eq a-val b-val) (is-eq a-val c-val) (not (is-eq a-val u0)))
))
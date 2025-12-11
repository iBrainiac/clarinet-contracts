;; --------------------------------------------------------------
;; Ballot Voting Contract 
;; Allows you to create a ballot with proposals, give voters the right to vote,
;; delegate votes, and vote for proposals. Finally, it can compute the winning proposal.
;; --------------------------------------------------------------

;; -----------------------------
;; 
;; -----------------------------

(define-data-var chairperson principal tx-sender)

(define-data-var proposal-count uint u0)

;; Voter struct:
;; weight: uint
;; voted: bool
;; delegate: principal
;; vote: uint

(define-map voters
  { voter: principal }
  {
    weight: uint,
    voted: bool,
    delegate: principal,
    vote: uint
  }
)

;; Proposal struct:
;; name: (buff 32)
;; voteCount: uint

(define-map proposals
  { id: uint }
  {
    name: (buff 32),
    voteCount: uint
  }
)

;; Prevent re-initialization
(define-data-var initialized bool false)


;; --------------------------------------------------------------
;; INITIALIZATION FUNCTION (replaces Solidity constructor)
;; Must be called ONCE by deployer
;; --------------------------------------------------------------

(define-public (init (proposalNames (list 100 (buff 32))))
  (begin
    (asserts! (is-eq (var-get initialized) false) (err u100))
    (var-set initialized true)

    (var-set chairperson tx-sender)

    ;; give the chairperson the right to vote
    (map-set voters
      { voter: tx-sender }
      {
        weight: u1,
        voted: false,
        delegate: tx-sender,
        vote: u0
      }
    )

    ;; register proposals
    (let
      (
        (count (len proposalNames))
      )
      (var-set proposal-count (to-uint count))

      ;; iterate list & insert proposals
      (fold
        (lambda (proposal-name idx)
          (map-set proposals
            { id: idx }
            {
              name: proposal-name,
              voteCount: u0
            }
          )
          (+ idx u1)
        )
        u0
        proposalNames
      )
    )

    (ok true)
  )
)


;; --------------------------------------------------------------
;; giveRightToVote
;; Only chairperson can call
;; --------------------------------------------------------------

(define-public (give-right-to-vote (voter principal))
  (begin
    (asserts! (is-eq tx-sender (var-get chairperson)) (err u101))

    (let
      (
        (voter-data (map-get? voters { voter: voter }))
      )

      (asserts! (is-some voter-data) (err u102))
      (let (
        (v (unwrap! voter-data (err u103)))
      )
        (asserts! (not (get voted v)) (err u104))
        (asserts! (is-eq (get weight v) u0) (err u105))

        (map-set voters
          { voter: voter }
          {
            weight: u1,
            voted: false,
            delegate: voter,
            vote: u0
          }
        )
        (ok true)
      )
    )
  )
)


;; --------------------------------------------------------------
;; delegate(to)
;; --------------------------------------------------------------

(define-public (delegate (to principal))
  (let
    (
      (sender-opt (map-get? voters { voter: tx-sender }))
      (delegate-opt (map-get? voters { voter: to }))
    )

    (asserts! (is-some sender-opt) (err u200))
    (asserts! (is-some delegate-opt) (err u201))

    (let
      (
        (sender (unwrap! sender-opt (err u202)))
        (delegatee (unwrap! delegate-opt (err u203)))
      )

      (asserts! (not (get voted sender)) (err u204))
      (asserts! (is-eq (get weight sender) u1) (err u205))

      ;; prevent self-delegation
      (asserts! (not (is-eq to tx-sender)) (err u206))

      ;; update sender
      (map-set voters
        { voter: tx-sender }
        {
          weight: (get weight sender),
          voted: true,
          delegate: to,
          vote: (get vote sender)
        }
      )

      ;; if delegate already voted → add weight to their proposal
      (if (get voted delegatee)
        (let
          (
            (proposal-id (get vote delegatee))
            (proposal (unwrap! (map-get? proposals { id: proposal-id }) (err u207)))
          )
          (map-set proposals
            { id: proposal-id }
            {
              name: (get name proposal),
              voteCount: (+ (get voteCount proposal) (get weight sender))
            }
          )
        )
        ;; else → increase their weight
        (map-set voters
          { voter: to }
          {
            weight: (+ (get weight delegatee) (get weight sender)),
            voted: false,
            delegate: (get delegate delegatee),
            vote: (get vote delegatee)
          }
        )
      )

      (ok true)
    )
  )
)


;; --------------------------------------------------------------
;; vote(proposal-id)
;; --------------------------------------------------------------

(define-public (vote (proposal-id uint))
  (let
    (
      (sender-opt (map-get? voters { voter: tx-sender }))
    )
    (asserts! (is-some sender-opt) (err u300))
    (let
      (
        (sender (unwrap! sender-opt (err u301)))
      )

      (asserts! (is-eq (get voted sender) false) (err u302))
      (asserts! (>= (get weight sender) u1) (err u303))

      ;; update sender
      (map-set voters
        { voter: tx-sender }
        {
          weight: (get weight sender),
          voted: true,
          delegate: (get delegate sender),
          vote: proposal-id
        }
      )

      ;; update proposal
      (let
        (
          (proposal (unwrap! (map-get? proposals { id: proposal-id }) (err u304)))
        )

        (map-set proposals
          { id: proposal-id }
          {
            name: (get name proposal),
            voteCount: (+ (get voteCount proposal) (get weight sender))
          }
        )
      )
      (ok true)
    )
  )
)


;; --------------------------------------------------------------
;; Read-only: Get winning proposal
;; --------------------------------------------------------------

(define-read-only (winning-proposal)
  (let
    (
      (count (var-get proposal-count))
    )
    (let
      (
        (result
          (fold
            (lambda (id best)
              (let
                (
                  (p (unwrap! (map-get? proposals { id: id }) (err u400)))
                  (best-p (unwrap! (map-get? proposals { id: best }) (err u401)))
                )
                (if (> (get voteCount p) (get voteCount best-p)) id best)
              )
            )
            u0
            (range u0 count)
          )
        )
      )
      result
    )
  )
)

;; --------------------------------------------------------------
;; Read-only: Get winner name
;; --------------------------------------------------------------

(define-read-only (winner-name)
  (let
    (
      (wp (winning-proposal))
    )
    (let
      (
        (proposal (unwrap! (map-get? proposals { id: wp }) (err u500)))
      )
      (get name proposal)
    )
  )
)

;; contracts/credit-trade-registry.clar
;; SPDX-License-Identifier: MIT
;; CreditTradeRegistry -- Clarity (Clarinet / Stacks) port of Solidity contract

(define-constant ERR_NOT_OWNER u100)
(define-constant ERR_ALREADY_INIT u101)
(define-constant ERR_NOT_REGISTERED u102)
(define-constant ERR_ALREADY_REGISTERED u103)
(define-constant ERR_TRADE_NOT_FOUND u104)
(define-constant ERR_TRADE_ALREADY_COMPLETED u105)
(define-constant ERR_INVALID_ARGS u106)

;; TradeStatus as uint constants
(define-constant TRADE-PENDING u0)
(define-constant TRADE-APPROVED u1)
(define-constant TRADE-COMPLETED u2)
(define-constant TRADE-REJECTED u3)
(define-constant TRADE-FAILED u4)

;; Owner and initialization guard
(define-data-var owner principal (ok (as-contract tx-sender))) ;; placeholder, set on init
(define-data-var initialized bool false)

;; Trade counter
(define-data-var trade-count uint u0)

;; Agent structure stored in a map keyed by principal
;; agent-type: (buff 32); registered: bool; registration-time: uint; trade-count: uint
(define-map agents
  { agent: principal }
  {
    agent-type: (buff 32),
    registered: bool,
    registration-time: uint,
    trade-count: uint
  }
)

;; Trades map keyed by id (uint)
;; fields: buyer, seller, credits, usdc, supervisor-fee, status, created-at, completed-at, txn-hash, temp-key
(define-map trades
  { id: uint }
  {
    buyer: principal,
    seller: principal,
    credits-amount: uint,
    usdc-amount: uint,
    supervisor-fee: uint,
    status: uint,
    created-at: uint,
    completed-at: uint,
    txn-hash: (buff 32),
    temp-key: (buff 64)
  }
)

;; ---------- Helpers ----------
(define-private (is-owner (p principal))
  (is-eq p (var-get owner))
)

;; ---------- Initialization ----------
;; call once to set owner and reset trade-count
(define-public (init)
  (begin
    (asserts! (is-eq (var-get initialized) false) (err ERR_ALREADY_INIT))
    (var-set initialized true)
    (var-set owner tx-sender)
    (var-set trade-count u0)
    (ok true)
  )
)

;; ---------- Owner-only register agent ----------
(define-public (register-agent (agent-addr principal) (agent-type (buff 32)))
  (begin
    (asserts! (is-eq tx-sender (var-get owner)) (err ERR_NOT_OWNER))
    (let ((existing (map-get? agents { agent: agent-addr })))
      (asserts! (is-none existing) (err ERR_ALREADY_REGISTERED))
      (map-set agents
        { agent: agent-addr }
        {
          agent-type: agent-type,
          registered: true,
          registration-time: (get-block-height),
          trade-count: u0
        }
      )
      (ok true)
    )
  )
)

;; ---------- Record a trade (after payment verification) ----------
(define-public (record-trade
               (buyer principal)
               (seller principal)
               (credits-amount uint)
               (usdc-amount uint)
               (supervisor-fee uint))
  (begin
    ;; require both agents registered
    (let ((b (map-get? agents { agent: buyer })))
      (asserts! (is-some b) (err ERR_NOT_REGISTERED))
    )
    (let ((s (map-get? agents { agent: seller })))
      (asserts! (is-some s) (err ERR_NOT_REGISTERED))
    )

    ;; increment trade counter
    (var-set trade-count (+ (var-get trade-count) u1))
    (let ((tid (var-get trade-count)))
      ;; store trade with status APPROVED (mirrors Solidity example)
      (map-set trades
        { id: tid }
        {
          buyer: buyer,
          seller: seller,
          credits-amount: credits-amount,
          usdc-amount: usdc-amount,
          supervisor-fee: supervisor-fee,
          status: TRADE-APPROVED,
          created-at: (get-block-height),
          completed-at: u0,
          txn-hash: (buff 32 0), ;; zeroed buffer
          temp-key: (buff 64 0)
        }
      )
      ;; update agent trade counts
      (let ((bdata (unwrap! (map-get? agents { agent: buyer }) (err ERR_NOT_REGISTERED)))
            (sdata (unwrap! (map-get? agents { agent: seller }) (err ERR_NOT_REGISTERED)))
      )
        (map-set agents
          { agent: buyer }
          {
            agent-type: (get agent-type bdata),
            registered: (get registered bdata),
            registration-time: (get registration-time bdata),
            trade-count: (+ (get trade-count bdata) u1)
          }
        )
        (map-set agents
          { agent: seller }
          {
            agent-type: (get agent-type sdata),
            registered: (get registered sdata),
            registration-time: (get registration-time sdata),
            trade-count: (+ (get trade-count sdata) u1)
          }
        )
      )
      ;; return trade id
      (ok tid)
    )
  )
)

;; ---------- Complete a trade ----------
(define-public (complete-trade (trade-id uint) (txn-hash (buff 32)) (temp-key (buff 64)))
  (let ((t (map-get? trades { id: trade-id })))
    (asserts! (is-some t) (err ERR_TRADE_NOT_FOUND))
    (let ((td (unwrap! t (err ERR_TRADE_NOT_FOUND))))
      (asserts! (is-eq (get status td) TRADE-COMPLETED) (ok true)) ;; noop if already completed? we'll check below
      ;; check not already completed
      (asserts! (not (is-eq (get status td) TRADE-COMPLETED)) (err ERR_TRADE_ALREADY_COMPLETED))
      (map-set trades
        { id: trade-id }
        {
          buyer: (get buyer td),
          seller: (get seller td),
          credits-amount: (get credits-amount td),
          usdc-amount: (get usdc-amount td),
          supervisor-fee: (get supervisor-fee td),
          status: TRADE-COMPLETED,
          created-at: (get created-at td),
          completed-at: (get-block-height),
          txn-hash: txn-hash,
          temp-key: temp-key
        }
      )
      (ok true)
    )
  )
)

;; ---------- Fail a trade ----------
(define-public (fail-trade (trade-id uint) (reason (buff 64)))
  (let ((t (map-get? trades { id: trade-id })))
    (asserts! (is-some t) (err ERR_TRADE_NOT_FOUND))
    (let ((td (unwrap! t (err ERR_TRADE_NOT_FOUND))))
      (map-set trades
        { id: trade-id }
        {
          buyer: (get buyer td),
          seller: (get seller td),
          credits-amount: (get credits-amount td),
          usdc-amount: (get usdc-amount td),
          supervisor-fee: (get supervisor-fee td),
          status: TRADE-FAILED,
          created-at: (get created-at td),
          completed-at: (get completed-at td),
          txn-hash: (get txn-hash td),
          temp-key: (get temp-key td)
        }
      )
      ;; (Optional) you can store 'reason' off-chain or in a separate map for reasons
      (ok true)
    )
  )
)

;; ---------- Read-only getters ----------

(define-read-only (get-trade (trade-id uint))
  (let ((t (map-get? trades { id: trade-id })))
    (match t
      trade (ok trade)
      none (err ERR_TRADE_NOT_FOUND)
    )
  )
)

(define-read-only (get-trade-status (trade-id uint))
  (let ((t (map-get? trades { id: trade-id })))
    (match t
      trade (ok (get status trade))
      none (err ERR_TRADE_NOT_FOUND)
    )
  )
)

(define-read-only (get-trade-txn-hash (trade-id uint))
  (let ((t (map-get? trades { id: trade-id })))
    (match t
      trade (ok (get txn-hash trade))
      none (err ERR_TRADE_NOT_FOUND)
    )
  )
)

(define-read-only (get-agent (agent-addr principal))
  (let ((a (map-get? agents { agent: agent-addr })))
    (match a
      agent (ok agent)
      none (err ERR_NOT_REGISTERED)
    )
  )
)

(define-read-only (is-agent-registered (agent-addr principal))
  (let ((a (map-get? agents { agent: agent-addr })))
    (ok (is-some a))
  )
)

(define-read-only (get-agent-trade-count (agent-addr principal))
  (let ((a (map-get? agents { agent: agent-addr })))
    (match a
      agent (ok (get trade-count agent))
      none (err ERR_NOT_REGISTERED)
    )
  )
)

(define-read-only (get-agent-type (agent-addr principal))
  (let ((a (map-get? agents { agent: agent-addr })))
    (match a
      agent (ok (get agent-type agent))
      none (err ERR_NOT_REGISTERED)
    )
  )
)

(define-read-only (get-trade-count)
  (ok (var-get trade-count))
)

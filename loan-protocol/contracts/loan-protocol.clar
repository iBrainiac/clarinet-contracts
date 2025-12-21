;; Bitcoin-USDT Decentralized Lending Protocol (Clarity 1 Compatible)
;; Simplified version using escrow functions instead of as-contract

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant PROTOCOL_TREASURY tx-sender)
(define-constant ASSET_BTC "BTC")
(define-constant ASSET_USDT "USDT")
(define-constant MIN_DURATION u1008)
(define-constant MAX_DURATION u5256000)
(define-constant MIN_AUCTION_DURATION u144)
(define-constant PROTOCOL_FEE_RATE u100)
(define-constant FEE_DENOMINATOR u100000)
(define-constant MIN_BTC_AMOUNT u1000000)
(define-constant MIN_USDT_AMOUNT u100000000)

;; Error codes
(define-constant ERR_INVALID_ASSET (err u401))
(define-constant ERR_SAME_ASSET (err u402))
(define-constant ERR_INVALID_AMOUNT (err u403))
(define-constant ERR_NO_INTEREST (err u404))
(define-constant ERR_INVALID_DURATION (err u405))
(define-constant ERR_AMOUNT_TOO_SMALL (err u409))
(define-constant ERR_LOAN_NOT_FOUND (err u501))
(define-constant ERR_AUCTION_ENDED (err u502))
(define-constant ERR_EXCEEDS_MAX_REPAYMENT (err u503))
(define-constant ERR_NOT_BETTER_BID (err u504))
(define-constant ERR_BID_NO_INTEREST (err u505))
(define-constant ERR_INVALID_LOAN_STATUS (err u508))

;; Data
(define-data-var loan-nonce uint u0)
(define-data-var bid-nonce uint u0)
(define-data-var protocol-principal principal tx-sender)

(define-map loans {loan-id: uint} {
  borrower: principal, collateral-asset: (string-ascii 10), collateral-amount: uint,
  borrow-asset: (string-ascii 10), borrow-amount: uint, max-repayment: uint,
  winning-repayment: uint, duration-blocks: uint, maturity-block: uint,
  auction-end-block: uint, status: (string-ascii 20),
  borrower-nft-id: uint, lender-nft-id: uint, protocol-fee-paid: uint
})

(define-map bids {bid-id: uint} {
  loan-id: uint, lender: principal, repayment-amount: uint, status: (string-ascii 20)
})

(define-map loan-winning-bid {loan-id: uint} {
  bid-id: uint, lender: principal, repayment-amount: uint
})

;; Helpers
(define-private (is-valid-asset (asset (string-ascii 10)))
  (or (is-eq asset ASSET_BTC) (is-eq asset ASSET_USDT)))

(define-private (get-min-amount (asset (string-ascii 10)))
  (if (is-eq asset ASSET_BTC) MIN_BTC_AMOUNT MIN_USDT_AMOUNT))

;; CREATE LOAN
(define-public (create-loan-auction
    (collateral-asset (string-ascii 10)) (collateral-amount uint)
    (borrow-asset (string-ascii 10)) (borrow-amount uint)
    (max-repayment uint) (duration-blocks uint) (auction-duration-blocks uint))
  (let (
    (loan-id (+ (var-get loan-nonce) u1))
    (maturity-block (+ burn-block-height duration-blocks))
    (auction-end (+ stacks-block-height auction-duration-blocks))
    (protocol-fee (/ (* collateral-amount PROTOCOL_FEE_RATE) FEE_DENOMINATOR))
  )
    (asserts! (is-valid-asset collateral-asset) ERR_INVALID_ASSET)
    (asserts! (is-valid-asset borrow-asset) ERR_INVALID_ASSET)
    (asserts! (not (is-eq collateral-asset borrow-asset)) ERR_SAME_ASSET)
    (asserts! (> max-repayment borrow-amount) ERR_NO_INTEREST)
    (asserts! (>= duration-blocks MIN_DURATION) ERR_INVALID_DURATION)
    (asserts! (<= duration-blocks MAX_DURATION) ERR_INVALID_DURATION)
    (asserts! (>= auction-duration-blocks MIN_AUCTION_DURATION) ERR_INVALID_DURATION)
    (asserts! (>= collateral-amount (get-min-amount collateral-asset)) ERR_AMOUNT_TOO_SMALL)
    (asserts! (>= borrow-amount (get-min-amount borrow-asset)) ERR_AMOUNT_TOO_SMALL)
    
    ;; Transfer fee
    (if (is-eq collateral-asset ASSET_BTC)
      (try! (contract-call? .mock-sbtc-v3 transfer protocol-fee tx-sender PROTOCOL_TREASURY none))
      (try! (contract-call? .mock-usdt-v3 transfer protocol-fee tx-sender PROTOCOL_TREASURY none)))
    
    ;; Lock collateral in escrow - user calls escrow-lock which transfers to THIS contract
    (if (is-eq collateral-asset ASSET_BTC)
      (try! (contract-call? .mock-sbtc-v3 escrow-lock collateral-amount (var-get protocol-principal)))
      (try! (contract-call? .mock-usdt-v3 escrow-lock collateral-amount (var-get protocol-principal))))
    
    ;; Mint NFT
    (let ((borrower-nft-id (try! (contract-call? .borrower-nft-v3 mint tx-sender loan-id
      collateral-asset collateral-amount borrow-asset max-repayment maturity-block))))
      
      (map-set loans {loan-id: loan-id} {
        borrower: tx-sender, collateral-asset: collateral-asset, collateral-amount: collateral-amount,
        borrow-asset: borrow-asset, borrow-amount: borrow-amount, max-repayment: max-repayment,
        winning-repayment: u0, duration-blocks: duration-blocks, maturity-block: maturity-block,
        auction-end-block: auction-end, status: "auction",
        borrower-nft-id: borrower-nft-id, lender-nft-id: u0, protocol-fee-paid: protocol-fee})
      
      (var-set loan-nonce loan-id)
      (print {event: "loan-created", loan-id: loan-id})
      (ok loan-id))))

;; PLACE BID
(define-public (place-bid (loan-id uint) (repayment-amount uint))
  (let (
    (loan (unwrap! (map-get? loans {loan-id: loan-id}) ERR_LOAN_NOT_FOUND))
    (current-best (map-get? loan-winning-bid {loan-id: loan-id}))
    (bid-id (+ (var-get bid-nonce) u1))
  )
    (asserts! (is-eq (get status loan) "auction") ERR_INVALID_LOAN_STATUS)
    (asserts! (<= stacks-block-height (get auction-end-block loan)) ERR_AUCTION_ENDED)
    (asserts! (<= repayment-amount (get max-repayment loan)) ERR_EXCEEDS_MAX_REPAYMENT)
    (asserts! (> repayment-amount (get borrow-amount loan)) ERR_BID_NO_INTEREST)
    (match current-best best
      (asserts! (< repayment-amount (get repayment-amount best)) ERR_NOT_BETTER_BID) true)
    
    ;; Refund previous bidder
    (match current-best best
      (if (is-eq (get borrow-asset loan) ASSET_BTC)
        (try! (contract-call? .mock-sbtc-v3 escrow-release (get borrow-amount loan) (get lender best)))
        (try! (contract-call? .mock-usdt-v3 escrow-release (get borrow-amount loan) (get lender best))))
      true)
    
    ;; Lock new bidder's tokens
    (if (is-eq (get borrow-asset loan) ASSET_BTC)
      (try! (contract-call? .mock-sbtc-v3 escrow-lock (get borrow-amount loan) (var-get protocol-principal)))
      (try! (contract-call? .mock-usdt-v3 escrow-lock (get borrow-amount loan) (var-get protocol-principal))))
    
    (map-set bids {bid-id: bid-id} {
      loan-id: loan-id, lender: tx-sender, repayment-amount: repayment-amount, status: "winning"})
    (map-set loan-winning-bid {loan-id: loan-id} {
      bid-id: bid-id, lender: tx-sender, repayment-amount: repayment-amount})
    
    (var-set bid-nonce bid-id)
    (print {event: "bid-placed", bid-id: bid-id, loan-id: loan-id})
    (ok bid-id)))

;; Read-only functions
(define-read-only (get-loan (loan-id uint))
  (map-get? loans {loan-id: loan-id}))

(define-read-only (get-winning-bid (loan-id uint))
  (map-get? loan-winning-bid {loan-id: loan-id}))

(define-read-only (get-loan-nonce)
  (var-get loan-nonce))
;; Borrower NFT Contract (Clarity 1)

(define-non-fungible-token borrower-position uint)
(define-data-var last-token-id uint u0)
(define-data-var contract-owner principal tx-sender)
(define-map authorized-minters principal bool)

(define-map token-metadata uint {
  loan-id: uint,
  collateral-asset: (string-ascii 10),
  collateral-amount: uint,
  debt-asset: (string-ascii 10),
  debt-amount: uint,
  maturity-block: uint
})

(define-read-only (get-last-token-id)
  (ok (var-get last-token-id)))

(define-read-only (get-token-uri (token-id uint))
  (ok (some "https://api.yourprotocol.com/borrower-nft/{id}")))

(define-read-only (get-owner (token-id uint))
  (ok (nft-get-owner? borrower-position token-id)))

(define-public (transfer (token-id uint) (sender principal) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender sender) (err u403))
    (nft-transfer? borrower-position token-id sender recipient)))

(define-public (mint (recipient principal) (loan-id uint) (collateral-asset (string-ascii 10))
    (collateral-amount uint) (debt-asset (string-ascii 10)) (debt-amount uint) (maturity-block uint))
  (let ((token-id (+ (var-get last-token-id) u1)))
    (asserts! (default-to false (map-get? authorized-minters tx-sender)) (err u401))
    (try! (nft-mint? borrower-position token-id recipient))
    (map-set token-metadata token-id {
      loan-id: loan-id, collateral-asset: collateral-asset, collateral-amount: collateral-amount,
      debt-asset: debt-asset, debt-amount: debt-amount, maturity-block: maturity-block})
    (var-set last-token-id token-id)
    (ok token-id)))

(define-public (burn (token-id uint))
  (begin
    (asserts! (default-to false (map-get? authorized-minters tx-sender)) (err u401))
    (nft-burn? borrower-position token-id (unwrap! (nft-get-owner? borrower-position token-id) (err u404)))))

(define-read-only (get-metadata (token-id uint))
  (map-get? token-metadata token-id))

(define-public (set-authorized-minter (account principal) (authorized bool))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) (err u401))
    (ok (map-set authorized-minters account authorized))))

;; StackStream Creator Registry Contract
;; Manages creator profiles, verification status, and content metadata

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-insufficient-funds (err u103))
(define-constant err-unauthorized (err u104))
(define-constant err-invalid-input (err u105))

;; Registration and fee constants
(define-constant registration-fee u5000000) ;; 5 STX in microSTX
(define-constant update-fee u500000) ;; 0.5 STX in microSTX
(define-constant verification-stake u10000000) ;; 10 STX in microSTX
(define-constant platform-fee-percent u50) ;; 0.5% = 50/10000

;; Data Variables
(define-data-var total-creators uint u0)
(define-data-var last-badge-id uint u0)
(define-data-var platform-treasury principal contract-owner)

;; Data Maps
(define-map creators 
    principal 
    {
        username: (string-ascii 50),
        bio: (string-utf8 500),
        profile-image-url: (string-utf8 256),
        is-verified: bool,
        reputation-score: uint,
        total-content: uint,
        total-revenue: uint,
        registered-at: uint,
        verification-stake: uint
    }
)

(define-map creator-usernames (string-ascii 50) principal)

(define-map creator-badges 
    uint 
    {
        creator: principal,
        badge-type: (string-ascii 20),
        issued-at: uint
    }
)

(define-map content-metadata
    {creator: principal, content-id: uint}
    {
        title: (string-utf8 200),
        description: (string-utf8 1000),
        content-hash: (buff 32),
        price: uint,
        access-count: uint,
        revenue: uint,
        created-at: uint,
        is-active: bool
    }
)

(define-map creator-content-count principal uint)

;; SIP-009 NFT Trait for Creator Badges
(define-non-fungible-token creator-badge uint)

;; Private Functions

(define-private (calculate-platform-fee (amount uint))
    (/ (* amount platform-fee-percent) u10000)
)

(define-private (validate-username (username (string-ascii 50)))
    (let ((username-len (len username)))
        (and (>= username-len u3) (<= username-len u50))
    )
)

(define-private (increment-reputation (creator principal) (points uint))
    (match (map-get? creators creator)
        creator-data 
            (map-set creators creator 
                (merge creator-data {reputation-score: (+ (get reputation-score creator-data) points)})
            )
        false
    )
)

;; Public Functions

;; Register as a creator
(define-public (register-creator (username (string-ascii 50)) 
                                 (bio (string-utf8 500)) 
                                 (profile-image-url (string-utf8 256)))
    (let (
        (caller tx-sender)
        (platform-fee (calculate-platform-fee registration-fee))
    )
        ;; Validate input
        (asserts! (validate-username username) err-invalid-input)
        (asserts! (is-none (map-get? creators caller)) err-already-exists)
        (asserts! (is-none (map-get? creator-usernames username)) err-already-exists)
        
        ;; Transfer registration fee
        (try! (stx-transfer? registration-fee caller (var-get platform-treasury)))
        
        ;; Create creator profile
        (map-set creators caller {
            username: username,
            bio: bio,
            profile-image-url: profile-image-url,
            is-verified: false,
            reputation-score: u100, ;; Starting reputation
            total-content: u0,
            total-revenue: u0,
            registered-at: stacks-block-height,
            verification-stake: u0
        })
        
        ;; Map username to creator
        (map-set creator-usernames username caller)
        
        ;; Initialize content count
        (map-set creator-content-count caller u0)
        
        ;; Increment total creators
        (var-set total-creators (+ (var-get total-creators) u1))
        
        (ok true)
    )
)

;; Update creator profile metadata
(define-public (update-profile (bio (string-utf8 500)) (profile-image-url (string-utf8 256)))
    (let (
        (caller tx-sender)
        (creator-data (unwrap! (map-get? creators caller) err-not-found))
    )
        ;; Transfer update fee
        (try! (stx-transfer? update-fee caller (var-get platform-treasury)))
        
        ;; Update profile using replace-at pattern
        (map-set creators caller 
            (merge creator-data {
                bio: bio,
                profile-image-url: profile-image-url
            })
        )
        
        (ok true)
    )
)

;; Verify identity by staking tokens
(define-public (verify-identity)
    (let (
        (caller tx-sender)
        (creator-data (unwrap! (map-get? creators caller) err-not-found))
    )
        ;; Check if already verified
        (asserts! (not (get is-verified creator-data)) err-already-exists)
        
        ;; Transfer verification stake to platform treasury (held for verification)
        (try! (stx-transfer? verification-stake caller (var-get platform-treasury)))
        
        ;; Update verification status
        (map-set creators caller 
            (merge creator-data {
                is-verified: true,
                verification-stake: verification-stake,
                reputation-score: (+ (get reputation-score creator-data) u500) ;; Bonus reputation
            })
        )
        
        ;; Mint creator badge NFT
        (let ((badge-id (+ (var-get last-badge-id) u1)))
            (try! (nft-mint? creator-badge badge-id caller))
            (map-set creator-badges badge-id {
                creator: caller,
                badge-type: "VERIFIED",
                issued-at: stacks-block-height
            })
            (var-set last-badge-id badge-id)
        )
        
        (ok true)
    )
)

;; Unverify and refund stake (admin only for now, can be extended)
(define-public (refund-verification-stake)
    (let (
        (caller tx-sender)
        (creator-data (unwrap! (map-get? creators caller) err-not-found))
        (stake-amount (get verification-stake creator-data))
    )
        ;; Check if verified
        (asserts! (get is-verified creator-data) err-unauthorized)
        (asserts! (> stake-amount u0) err-insufficient-funds)
        
        ;; Refund stake from platform treasury
        (try! (stx-transfer? stake-amount (var-get platform-treasury) caller))
        
        ;; Update verification status
        (map-set creators caller 
            (merge creator-data {
                is-verified: false,
                verification-stake: u0
            })
        )
        
        (ok true)
    )
)

;; Add content metadata
(define-public (add-content (title (string-utf8 200))
                           (description (string-utf8 1000))
                           (content-hash (buff 32))
                           (price uint))
    (let (
        (caller tx-sender)
        (creator-data (unwrap! (map-get? creators caller) err-not-found))
        (current-count (default-to u0 (map-get? creator-content-count caller)))
        (content-id (+ current-count u1))
    )
        ;; Validate price
        (asserts! (> price u0) err-invalid-input)
        
        ;; Store content metadata
        (map-set content-metadata {creator: caller, content-id: content-id} {
            title: title,
            description: description,
            content-hash: content-hash,
            price: price,
            access-count: u0,
            revenue: u0,
            created-at: stacks-block-height,
            is-active: true
        })
        
        ;; Update counts
        (map-set creator-content-count caller content-id)
        (map-set creators caller 
            (merge creator-data {
                total-content: (+ (get total-content creator-data) u1)
            })
        )
        
        ;; Add reputation for content creation
        (increment-reputation caller u10)
        
        (ok content-id)
    )
)

;; Update content status (active/inactive)
(define-public (toggle-content-status (content-id uint))
    (let (
        (caller tx-sender)
        (content-key {creator: caller, content-id: content-id})
        (content-data (unwrap! (map-get? content-metadata content-key) err-not-found))
    )
        (map-set content-metadata content-key
            (merge content-data {
                is-active: (not (get is-active content-data))
            })
        )
        (ok true)
    )
)

;; Record content access (called by payment contracts)
(define-public (record-content-access (creator principal) (content-id uint) (revenue-amount uint))
    (let (
        (content-key {creator: creator, content-id: content-id})
        (content-data (unwrap! (map-get? content-metadata content-key) err-not-found))
        (creator-data (unwrap! (map-get? creators creator) err-not-found))
    )
        ;; Update content metrics
        (map-set content-metadata content-key
            (merge content-data {
                access-count: (+ (get access-count content-data) u1),
                revenue: (+ (get revenue content-data) revenue-amount)
            })
        )
        
        ;; Update creator revenue
        (map-set creators creator
            (merge creator-data {
                total-revenue: (+ (get total-revenue creator-data) revenue-amount)
            })
        )
        
        ;; Add reputation for successful sale
        (increment-reputation creator u5)
        
        (ok true)
    )
)

;; Read-only Functions

(define-read-only (get-creator (creator principal))
    (ok (map-get? creators creator))
)

(define-read-only (get-creator-by-username (username (string-ascii 50)))
    (match (map-get? creator-usernames username)
        creator-principal (ok (map-get? creators creator-principal))
        (ok none)
    )
)

(define-read-only (get-content-metadata (creator principal) (content-id uint))
    (ok (map-get? content-metadata {creator: creator, content-id: content-id}))
)

(define-read-only (get-creator-content-count (creator principal))
    (ok (default-to u0 (map-get? creator-content-count creator)))
)

(define-read-only (get-total-creators)
    (ok (var-get total-creators))
)

(define-read-only (get-creator-badge (badge-id uint))
    (ok (map-get? creator-badges badge-id))
)

(define-read-only (get-badge-owner (badge-id uint))
    (ok (nft-get-owner? creator-badge badge-id))
)

;; Admin Functions

(define-public (set-platform-treasury (new-treasury principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set platform-treasury new-treasury)
        (ok true)
    )
)

(define-public (adjust-creator-reputation (creator principal) (new-score uint))
    (let (
        (creator-data (unwrap! (map-get? creators creator) err-not-found))
    )
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set creators creator 
            (merge creator-data {reputation-score: new-score})
        )
        (ok true)
    )
)

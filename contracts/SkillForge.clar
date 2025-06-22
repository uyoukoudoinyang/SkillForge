;; Constants
(define-constant SKILL_POINT_TREASURY u2200000)
(define-constant BASE_PRACTICE_REWARD u20)
(define-constant PROFICIENCY_BONUS u8)
(define-constant MAX_PROFICIENCY_LEVEL u15)
(define-constant ERR_INVALID_PRACTICE u1)
(define-constant ERR_NO_SKILL_POINTS u2)
(define-constant ERR_TREASURY_DEPLETED u3)
(define-constant BLOCKS_PER_SKILL_CYCLE u1200)
(define-constant MASTERY_MULTIPLIER u5)
(define-constant MIN_MASTERY_DURATION u960)
(define-constant MASTERY_ABANDONMENT_PENALTY u20)

;; Data Variables
(define-data-var total-skill-points-awarded uint u0)
(define-data-var total-practice-sessions uint u0)
(define-data-var skill-mentor principal tx-sender)

;; Data Maps
(define-map practitioner-sessions principal uint)
(define-map practitioner-skill-points principal uint)
(define-map practice-session-start principal uint)
(define-map practitioner-proficiency principal uint)
(define-map practitioner-last-practice principal uint)
(define-map practitioner-mastery-tokens principal uint)
(define-map practitioner-mastery-start-block principal uint)

;; Public Functions

(define-public (initiate-practice-session (difficulty uint))
  (let
    (
      (practitioner tx-sender)
    )
    (asserts! (> difficulty u0) (err ERR_INVALID_PRACTICE))
    (map-set practice-session-start practitioner burn-block-height)
    (ok true)
  )
)

(define-public (conclude-practice-session (difficulty uint))
  (let
    (
      (practitioner tx-sender)
      (start-block (default-to u0 (map-get? practice-session-start practitioner)))
      (blocks-practicing (- burn-block-height start-block))
      (last-practice-block (default-to u0 (map-get? practitioner-last-practice practitioner)))
      (proficiency-level (default-to u0 (map-get? practitioner-proficiency practitioner)))
      (capped-proficiency (if (<= proficiency-level MAX_PROFICIENCY_LEVEL) proficiency-level MAX_PROFICIENCY_LEVEL))
      (reward-amount (+ BASE_PRACTICE_REWARD (* capped-proficiency PROFICIENCY_BONUS)))
    )
    (asserts! (and (> start-block u0) (>= blocks-practicing difficulty)) (err ERR_INVALID_PRACTICE))
    (map-set practitioner-sessions practitioner (+ (default-to u0 (map-get? practitioner-sessions practitioner)) u1))
    (map-set practitioner-skill-points practitioner (+ (default-to u0 (map-get? practitioner-skill-points practitioner)) reward-amount))
    (if (< (- burn-block-height last-practice-block) BLOCKS_PER_SKILL_CYCLE)
      (map-set practitioner-proficiency practitioner (+ proficiency-level u1))
      (map-set practitioner-proficiency practitioner u1)
    )
    (map-set practitioner-last-practice practitioner burn-block-height)
    (var-set total-practice-sessions (+ (var-get total-practice-sessions) u1))
    (var-set total-skill-points-awarded (+ (var-get total-skill-points-awarded) reward-amount))
    (asserts! (<= (var-get total-skill-points-awarded) SKILL_POINT_TREASURY) (err ERR_TREASURY_DEPLETED))
    (ok reward-amount)
  )
)

(define-public (collect-skill-rewards)
  (let
    (
      (practitioner tx-sender)
      (point-balance (default-to u0 (map-get? practitioner-skill-points practitioner)))
    )
    (asserts! (> point-balance u0) (err ERR_NO_SKILL_POINTS))
    (map-set practitioner-skill-points practitioner u0)
    (ok point-balance)
  )
)

;; Mastery Features

(define-public (commit-mastery-tokens (amount uint))
  (let
    (
      (practitioner tx-sender)
    )
    (asserts! (> amount u0) (err ERR_INVALID_PRACTICE))
    (asserts! (>= (var-get total-skill-points-awarded) amount) (err ERR_TREASURY_DEPLETED))
    (map-set practitioner-mastery-tokens practitioner amount)
    (map-set practitioner-mastery-start-block practitioner burn-block-height)
    (var-set total-skill-points-awarded (- (var-get total-skill-points-awarded) amount))
    (ok amount)
  )
)

(define-public (release-mastery-tokens)
  (let
    (
      (practitioner tx-sender)
      (mastery-amount (default-to u0 (map-get? practitioner-mastery-tokens practitioner)))
      (mastery-start-block (default-to u0 (map-get? practitioner-mastery-start-block practitioner)))
      (blocks-mastering (- burn-block-height mastery-start-block))
      (penalty (if (< blocks-mastering MIN_MASTERY_DURATION) (/ (* mastery-amount MASTERY_ABANDONMENT_PENALTY) u100) u0))
      (final-amount (- mastery-amount penalty))
    )
    (asserts! (> mastery-amount u0) (err ERR_NO_SKILL_POINTS))
    (map-set practitioner-mastery-tokens practitioner u0)
    (map-set practitioner-mastery-start-block practitioner u0)
    (var-set total-skill-points-awarded (+ (var-get total-skill-points-awarded) final-amount))
    (ok final-amount)
  )
)

;; Read-Only Functions

(define-read-only (get-practice-session-count (user principal))
  (default-to u0 (map-get? practitioner-sessions user))
)

(define-read-only (get-skill-point-balance (user principal))
  (default-to u0 (map-get? practitioner-skill-points user))
)

(define-read-only (get-proficiency-level (user principal))
  (default-to u0 (map-get? practitioner-proficiency user))
)

(define-read-only (get-skill-platform-analytics)
  {
    total-practice-sessions: (var-get total-practice-sessions),
    total-skill-points-awarded: (var-get total-skill-points-awarded)
  }
)

;; Private Functions

(define-private (is-skill-mentor)
  (is-eq tx-sender (var-get skill-mentor))
)

;; MentorChain: Decentralized Mentorship Matching Platform
;; Version: 1.0.0
;; Connects experienced mentors with mentees for professional development and career guidance

(define-data-var program-coordinator principal tx-sender)

(define-map mentor-profiles
  { mentor-id: uint }
  {
    guide: principal,
    session-fee: uint,
    expertise-area: (string-ascii 50),
    mentor-background: (string-ascii 500),
    mentoring-experience: uint,
    certified: bool
  })

(define-map mentorship-sessions
  { mentor-id: uint, session-id: uint }
  {
    mentee: principal,
    session-time: uint,
    session-type: (string-ascii 20)
  })

(define-data-var next-mentor-id uint u1)

(define-map session-tracker
  { mentor-id: uint }
  { sessions: uint })

;; Register as a mentor
(define-public (register-mentor (expertise-input (string-ascii 50)) (background-input (string-ascii 500)) (experience-input uint) (fee-input uint))
  (let
    (
      (mentor-id (var-get next-mentor-id))
      (session-id u0)
      (expertise expertise-input)
      (background background-input)
      (experience experience-input)
      (fee fee-input)
    )
    ;; Input validation
    (asserts! (> fee u0) (err u1))
    (asserts! (> (len expertise) u0) (err u5))
    (asserts! (> (len background) u0) (err u6))
    (asserts! (> experience u0) (err u7))
    
    (map-set mentor-profiles
      { mentor-id: mentor-id }
      {
        guide: tx-sender,
        session-fee: fee,
        expertise-area: expertise,
        mentor-background: background,
        mentoring-experience: experience,
        certified: false
      }
    )
    (map-set mentorship-sessions
      { mentor-id: mentor-id, session-id: session-id }
      {
        mentee: tx-sender,
        session-time: mentor-id,
        session-type: "registered"
      }
    )
    (map-set session-tracker
      { mentor-id: mentor-id }
      { sessions: u1 }
    )
    (var-set next-mentor-id (+ mentor-id u1))
    (ok mentor-id)
  ))

;; Book a mentorship session
(define-public (book-mentorship (mentor-id-input uint))
  (let
    (
      (mentor-id mentor-id-input)
      (mentor-info (unwrap! (map-get? mentor-profiles { mentor-id: mentor-id }) (err u2)))
      (fee (get session-fee mentor-info))
      (guide (get guide mentor-info))
      (session-data (default-to { sessions: u0 } (map-get? session-tracker { mentor-id: mentor-id })))
      (session-id (get sessions session-data))
      (new-session-id (+ session-id u1))
    )
    ;; Input validation
    (asserts! (> mentor-id u0) (err u8))
    (asserts! (not (is-eq tx-sender guide)) (err u3))
    
    (try! (stx-transfer? fee tx-sender guide))
    (map-set mentorship-sessions
      { mentor-id: mentor-id, session-id: session-id }
      {
        mentee: tx-sender,
        session-time: (var-get next-mentor-id),
        session-type: "booked"
      }
    )
    (map-set session-tracker
      { mentor-id: mentor-id }
      { sessions: new-session-id }
    )
    (ok true)
  ))

;; Certify a mentor (coordinator only)
(define-public (certify-mentor (mentor-id-input uint))
  (let
    (
      (mentor-id mentor-id-input)
      (mentor-info (unwrap! (map-get? mentor-profiles { mentor-id: mentor-id }) (err u2)))
      (session-data (default-to { sessions: u0 } (map-get? session-tracker { mentor-id: mentor-id })))
      (session-id (get sessions session-data))
      (new-session-id (+ session-id u1))
    )
    ;; Input validation
    (asserts! (> mentor-id u0) (err u8))
    (asserts! (is-eq tx-sender (var-get program-coordinator)) (err u4))
    
    (map-set mentor-profiles
      { mentor-id: mentor-id }
      (merge mentor-info { certified: true })
    )
    (map-set mentorship-sessions
      { mentor-id: mentor-id, session-id: session-id }
      {
        mentee: (get guide mentor-info),
        session-time: (var-get next-mentor-id),
        session-type: "certified"
      }
    )
    (map-set session-tracker
      { mentor-id: mentor-id }
      { sessions: new-session-id }
    )
    (ok true)
  ))

;; Get mentor profile
(define-read-only (get-mentor (mentor-id uint))
  (map-get? mentor-profiles { mentor-id: mentor-id }))

;; Get mentorship session record
(define-read-only (get-session-record (mentor-id uint) (session-id uint))
  (map-get? mentorship-sessions { mentor-id: mentor-id, session-id: session-id }))

;; Get total sessions for a mentor
(define-read-only (get-session-count (mentor-id uint))
  (let
    (
      (session-data (default-to { sessions: u0 } (map-get? session-tracker { mentor-id: mentor-id })))
    )
    (get sessions session-data)
  ))

;; Get program stats
(define-read-only (get-program-stats)
  {
    coordinator: (var-get program-coordinator),
    total-mentors: (- (var-get next-mentor-id) u1)
  })
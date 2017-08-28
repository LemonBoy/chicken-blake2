(use test)
(use message-digest-bv blake2)

(test-group "Parameter validation"
  (test-error "zero length" (blake2b-primitive length: 0))
  (test-error "negative length" (blake2b-primitive length: -1))
  (test-error "inexact length" (blake2b-primitive length: 1.0))
  (test-error "huge length" (blake2b-primitive length: 1000))
  (test-error "empty key" (blake2b-primitive key: ""))
  (test-error "huge key" (blake2b-primitive key: (make-string 100)))
)

(test-group "RFC7693 examples"
  (test "blake2b"
        "ba80a53f981c4d0d6a2797b69f12f6e94c212f14685ac4b74b12bb6fdbffa2d17d87c5392aab792dc252d5de4533cc9518d38aa8dbf1925ab92386edd4009923"
        (message-digest-string (blake2b-primitive) "abc"))
  (test "blake2s"
        "508c5e8c327c14e2e1a72ba34eeb452f37458b209ed63a294d999b4c86675982"
        (message-digest-string (blake2s-primitive) "abc"))
)

(test-exit)

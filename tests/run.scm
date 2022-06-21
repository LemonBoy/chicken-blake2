(cond-expand
    (chicken-4
     (use test)
     (use message-digest message-digest-bv blake2 modular-arithmetic srfi-4 srfi-42))
    (chicken-5
     (import scheme (chicken base) (chicken bitwise) (chicken blob) (chicken foreign) blake2 message-digest modular-arithmetic srfi-4 srfi-42 test))
    (else (error "Unsupported chicken version.")))

(test-group "Parameter validation"
  (test-error "zero length" (blake2b-primitive length: 0))
  (test-error "negative length" (blake2b-primitive length: -1))
  (test-error "inexact length" (blake2b-primitive length: 1.0))
  (test-error "huge length" (blake2b-primitive length: 1000))
  (test-error "empty key" (blake2b-primitive key: ""))
  (test-error "huge key" (blake2b-primitive key: (make-string 100)))
)

(test-group "RFC7693 examples"
  (test "blake2b with message-digest-string"
        "ba80a53f981c4d0d6a2797b69f12f6e94c212f14685ac4b74b12bb6fdbffa2d17d87c5392aab792dc252d5de4533cc9518d38aa8dbf1925ab92386edd4009923"
        (message-digest-string (blake2b-primitive) "abc"))
  (test "blake2s with message-digest-string"
        "508c5e8c327c14e2e1a72ba34eeb452f37458b209ed63a294d999b4c86675982"
        (message-digest-string (blake2s-primitive) "abc"))
  (test "blake2b with message-digest-file"
        "ba80a53f981c4d0d6a2797b69f12f6e94c212f14685ac4b74b12bb6fdbffa2d17d87c5392aab792dc252d5de4533cc9518d38aa8dbf1925ab92386edd4009923"
        (message-digest-file (blake2b-primitive) "testfile.txt"))
  (test "blake2s with message-digest-file"
        "508c5e8c327c14e2e1a72ba34eeb452f37458b209ed63a294d999b4c86675982"
        (message-digest-file (blake2s-primitive) "testfile.txt"))
)

(test-group "RFC7693 Appendix E self-tests"
  (message-digest-result-form 'u8vector)
  (define (selftest-seq size seed)
    (let* ((out (make-u8vector size))
	   (uintmod (expt 2 32))
	   (uint+ (mod+ uintmod))
	   (uint* (mod* uintmod)))
      (let loop ((i 0) (a (uint* #xdead4bad seed)) (b 1))
	(when (< i size)
	  (let ((t (uint+ a b)))
	    (set! (u8vector-ref out i) (arithmetic-shift t -24))
	    (loop (add1 i) b t)))) out))

  (define (selftest-compute primitive md-lens in-lens)
    (let ((context2 (setup-message-digest (primitive length: 32))))
      (do-ec
       (:vector out-len md-lens)
       (:vector in-len in-lens)
       (let ((inseq (selftest-seq in-len in-len)))
	 (message-digest-update-u8vector context2
					 (message-digest-u8vector (primitive length: out-len) inseq))
	 (message-digest-update-u8vector context2
					 (message-digest-u8vector (primitive length: out-len key: (u8vector->blob (selftest-seq out-len out-len))) inseq))))
      (finalize-message-digest context2)))

  (test "blake2b selftest (RFC 7693, Appendix E)"
	#u8(
	    #xC2 #x3A #x78 #x00 #xD9 #x81 #x23 #xBD
		 #x10 #xF5 #x06 #xC6 #x1E #x29 #xDA #x56
		 #x03 #xD7 #x63 #xB8 #xBB #xAD #x2E #x73
		 #x7F #x5E #x76 #x5A #x7B #xCC #xD4 #x75
		 )
	(selftest-compute
	 blake2b-primitive
	 #(20 32 48 64)
	 #(0 3 128 129 255 1024)))

  (test "blake2s selftest"
	#u8(#x6A #x41 #x1F #x08 #xCE #x25 #xAD #xCD
		 #xFB #x02 #xAB #xA6 #x41 #x45 #x1C #xEC
		 #x53 #xC5 #x98 #xB2 #x4F #x4F #xC7 #x87
		 #xFB #xDC #x88 #x79 #x7F #x4C #x1D #xFE)
	(selftest-compute
	 blake2s-primitive
	 #(16 20 28 32)
	 #(0  3  64 65 255 1024))))

(test-exit)

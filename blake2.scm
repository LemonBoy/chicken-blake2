(module blake2
  (blake2s-primitive blake2b-primitive)
  (import scheme chicken foreign)

(use message-digest-primitive)
(import-for-syntax chicken)

#>
#include "ref/blake2.h"
<#

(define-syntax define-impl
  (er-macro-transformer
    (lambda (f r c)
      (let-optionals (map strip-syntax (cdr f))
        ((prefix #f) (state-size 0) (digest-length 0) (key-length 0) (block-size 0))
        (let ((primitive-name (symbol-append prefix '-primitive))
              (init-proc      (symbol-append prefix '_init))
              (init-key-proc  (symbol-append prefix '_init_key))
              (update-proc    (symbol-append prefix '_update))
              (final-proc     (symbol-append prefix '_final)))
          `(define ,primitive-name
             (lambda (#!key (length ,digest-length) key)
               (unless (and (exact? length) (> length 0) (<= length ,digest-length))
                 (error ',primitive-name "the digest length is out of bounds" 1 ,digest-length))
               (when key
                 (unless (or (string? key) (blob? key))
                   (error ',primitive-name "the key parameter must be a blob or a string"))
                 (unless (and (> (##sys#size key) 0) (<= (##sys#size key) ,key-length))
                   (error ',primitive-name "the key parameter is too long or too short")))
               (make-message-digest-primitive
                 ; size of the state block
                 ,state-size
                 ; length of the digest
                 length
                 ; init
                 (if key
                     (lambda (ctx)
                       ((foreign-lambda int ,init-key-proc c-pointer size_t scheme-pointer size_t) ctx length key (##sys#size key)))
                   (lambda (ctx)
                     ((foreign-lambda int ,init-proc c-pointer size_t) ctx length)))
                 ; update
                 (foreign-lambda int ,update-proc c-pointer scheme-pointer size_t)
                 ; finalize
                 (lambda (ctx out)
                   ((foreign-lambda int ,final-proc c-pointer scheme-pointer size_t) ctx out length))
                 ; block size
                 ,block-size
                 ; name
                 ',primitive-name
                 ; raw-update
                 (foreign-lambda int ,update-proc c-pointer c-pointer size_t)))))))))

(define-impl blake2s
  (foreign-type-size "blake2s_state")
  (foreign-value "BLAKE2S_OUTBYTES" unsigned-int)
  (foreign-value "BLAKE2S_KEYBYTES" unsigned-int)
  (foreign-value "BLAKE2S_BLOCKBYTES" unsigned-int))

(define-impl blake2b
  (foreign-type-size "blake2b_state")
  (foreign-value "BLAKE2B_OUTBYTES" unsigned-int)
  (foreign-value "BLAKE2B_KEYBYTES" unsigned-int)
  (foreign-value "BLAKE2B_BLOCKBYTES" unsigned-int))
)

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
        ((name #f) (init #f) (init-key #f) (update #f) (final #f) (state-size 0) (digest-length 0) (key-length 0) (block-size 0))
        `(define ,(symbol-append name '-primitive)
           (lambda (#!key (length ,digest-length) key)
             (unless (and (exact? length) (> length 0) (<= length ,digest-length))
               (error "the digest length is invalid"))
             (when key
               (unless (or (string? key) (blob? key))
                 (error "the key parameter must be a blob or a string"))
               (unless (and (> (##sys#size key) 0) (<= (##sys#size key) ,key-length))
                 (error "the key parameter is too long or too short")))
             (make-message-digest-primitive
               ; size of the state block
               ,state-size
               ; length of the digest
               length
               ; init
               (if key
                   (lambda (ctx)
                     ((foreign-lambda int ,init-key c-pointer size_t scheme-pointer size_t) ctx length key (##sys#size key)))
                   (lambda (ctx)
                     ((foreign-lambda int ,init c-pointer size_t) ctx length)))
               ; update
               (foreign-lambda int ,update c-pointer scheme-pointer size_t)
               ; finalize
               (lambda (ctx out)
                 ((foreign-lambda int ,final c-pointer scheme-pointer size_t) ctx out length))
               ; block size
               ,block-size
               ; name
               ',(symbol-append name '-primitive))))))))

(define-impl blake2s
  blake2s_init blake2s_init_key blake2s_update blake2s_final
  (foreign-type-size "blake2s_state")
  (foreign-value "BLAKE2S_OUTBYTES" unsigned-int)
  (foreign-value "BLAKE2S_KEYBYTES" unsigned-int)
  (foreign-value "BLAKE2S_BLOCKBYTES" unsigned-int))

(define-impl blake2b
  blake2b_init blake2b_init_key blake2b_update blake2b_final
  (foreign-type-size "blake2b_state")
  (foreign-value "BLAKE2B_OUTBYTES" unsigned-int)
  (foreign-value "BLAKE2B_KEYBYTES" unsigned-int)
  (foreign-value "BLAKE2B_BLOCKBYTES" unsigned-int))
)

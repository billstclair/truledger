(in-package :crypto-tests)

(rtest:deftest make-digest.error
  (handler-case (crypto:make-digest :error)
    (crypto:unsupported-digest () :ok)
    (:no-error () :error))
  :ok)

(rtest:deftest digest-length.error
  (handler-case (crypto:digest-length :error)
    (crypto:unsupported-digest () :ok)
    (:no-error () :error))
  :ok)

(rtest:deftest produce-digest.buffer-space
  (let ((sequence (make-array 0 :element-type '(unsigned-byte 8))))
    (dolist (digest (crypto:list-all-digests) :ok)
      (let* ((digest-length (crypto:digest-length digest))
             (buffer (make-array (1- digest-length)
                                 :element-type '(unsigned-byte 8))))
        (handler-case (crypto:digest-sequence digest sequence
                                              :digest buffer
                                              :digest-start 0)
          (crypto:insufficient-buffer-space () :ok)
          (:no-error () (return :error))))))
  :ok)

(rtest:deftest produce-digest.using-buffers
  (let ((sequence (make-array 0 :element-type '(unsigned-byte 8))))
    (dolist (digest (crypto:list-all-digests) :ok)
      (let* ((digest-length (crypto:digest-length digest))
             (buffer (make-array digest-length
                                 :element-type '(unsigned-byte 8)))
             (returned (crypto:digest-sequence digest sequence
                                               :digest buffer
                                               :digest-start 0)))
        (unless (eq returned buffer)
          (return :error)))))
  :ok)

#.(loop for digest in (crypto:list-all-digests)
        collect `(rtest:deftest ,digest
                   (run-test-vector-file ',digest *digest-tests*) t) into forms
        finally (return `(progn ,@forms)))

(rtest:deftest clean-symbols.digest
  (loop with n-digests = (length (crypto:list-all-digests))
     for s being each symbol of :crypto
     when (crypto::digestp s)
     count s into computed-n-digests
     finally (return (= n-digests computed-n-digests)))
  t)

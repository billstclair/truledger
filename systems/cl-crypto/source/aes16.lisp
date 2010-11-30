;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                  AES Implementation in Common Lisp
;;;; *                  Optimized for 16-bit math on 32-bit CL
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: mrbug@rayservers.net
;;;
;;; This is a version of the AES (Rijndael) encryption algorithm coded
;;; in pure Common Lisp, and restructured to use 16-bit fixnum arithmetic
;;; in order to avoid bignums for 32-bit quantities using more than 29 bits
;;; on 32-bit Common Lisp implementations.
;;;
;;; I have looked to many other examples of AES implementations in preparing
;;; this one, and have drawn on all to varying degrees.
;;;
;;; I have written all main routines based on the current FIPS-197 spec,
;;; but have borrowed some optimizations from Christophe Devine's
;;; C version, and some utilities from Joern Inge Vestgaarden's aes32.lisp
;;; for SBCL (by a good measure the fastest 32-bit AES I have found for CL).
;;;
;;; On my test systems, this 16-bit implementation runs about 2x as slow
;;; as Vestgaarden's aes32.lisp under 64-bit SBCL, and about 2x as fast
;;; as Vestgaarden's aes32.lisp under 64-bit Clozure CL.  Under 32-bit SBCL,
;;; Vestgaarden's aes32.lisp is 3x as fast as this 16-bit AES, but under
;;; 32-bit Clozure CL, this 16-bit AES is 16x as fast as Vestgaarden's
;;; aes32.lisp.
;;;
;;; In other words, even though this 16-bit AES uses roughly 2x as many
;;; operations as a simple 32-bit approach, it is an order of magnitude more
;;; efficient on non-SBCL 32-bit systems where bignums are often
;;; required when dealing with 32-bit Rijndael field operations.
;;;
;;; NOTE: using constants for AES tables because this is one case where
;;; we really want Lisp to enforce read-only state on the values.  And
;;; using the define-constant macro to get around SBCL's special treatment
;;; of constants, as per the SBCL manual and Vestgaarden's code. 
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  13 AUG 2010 File created.  (mrbug)
;;;              TODO: some macro cleanup, packaging, etc.
;;;
;;;  14 AUG 2010 Removed all inlines, converted to macros (mrbug)
;;;
;;;  18 OCT 2010 Moved some macros to macros.lisp (mrbug)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :cl-crypto)

;;;
;;; Local declaims
;;;
(declaim (type (simple-array uint-8)
	       +fsb+ +rsb+)
	 (type (simple-array uint-16)
	       +ft0+ +ft1+ +ft2+ +ft3+
	       +rt0+ +rt1+ +rt2+ +rt3+)
	 (type (simple-array)
	       +kt0+ +kt1+ +kt2+ +kt3+))


;;;
;;; Classes
;;;

(defclass aes-key ()
  ((num-rounds
    :type uint-8
    :accessor num-rounds
    :initarg :num-rounds)
   (forward-key
    :type (simple-array uint-16)
    :accessor forward-key-of
    :initarg :forward-key)
   (reverse-key
    :type (simple-array uint-16)
    :accessor reverse-key-of
    :initarg :reverse-key)))


;;;
;;; Macros
;;;

(defmacro array-rotate-uint-16-pairs-right (in)
  "Returns new array with right byte rotation
  across two adjacent uint-16 values"
  ;; This is a macro since it is called when making constants
  (with-gensyms (gnum-words gout gi gw0 gw1)
    `(let* ((,gnum-words (array-total-size ,in))
            (,gout (make-array ,gnum-words :element-type 'uint-16)))
       (do ((,gi 0 (+ 2 ,gi)))
           ((>= ,gi ,gnum-words))
         (let* ((,gw1 (aref ,in ,gi))
                (,gw0 (aref ,in (1+ ,gi))))
           (setf (aref ,gout ,gi) 
                 (logior (ash (ldb (byte 8 0) ,gw0) 8)
                         (ldb (byte 8 8) ,gw1)))
           (setf (aref ,gout (1+ ,gi)) 
                 (logior (ash (ldb (byte 8 0) ,gw1) 8)
                         (ldb (byte 8 8) ,gw0)))))
       ,gout)))

(defmacro gen-k-table (in)
  "Generate helper tables for reverse key schedule using equivalent
inverse algorithm"
  ;; This is a macro since it is called when making constants
  (with-gensyms (gsize gout)
    `(let* ((,gsize (/ (array-total-size ,in) 2))
	    (,gout (make-array ,gsize)))
       (dotimes (i ,gsize)
	 (setf (aref ,gout i)
	       (logior (ash (aref ,in (* 2 (aref +fsb+ i))) 16)
		       (aref ,in (1+ (* 2 (aref +fsb+ i)))))))
       ,gout)))



;;;
;;; Constants
;;;


;; 128-bit (16-byte, 4-word) block size
(defconstant +block-words+ 4)

(define-constant +rcon+
    '(#x01000000 #x02000000 #x04000000 #x08000000 #x10000000
      #x20000000 #x40000000 #x80000000 #x1B000000 #x36000000))

(define-constant +fsb+
    (make-array
     256 :element-type 'uint-8
     :initial-contents
     '(#x63 #x7C #x77 #x7B #xF2 #x6B #x6F #xC5 
       #x30 #x01 #x67 #x2B #xFE #xD7 #xAB #x76 
       #xCA #x82 #xC9 #x7D #xFA #x59 #x47 #xF0 
       #xAD #xD4 #xA2 #xAF #x9C #xA4 #x72 #xC0 
       #xB7 #xFD #x93 #x26 #x36 #x3F #xF7 #xCC 
       #x34 #xA5 #xE5 #xF1 #x71 #xD8 #x31 #x15 
       #x04 #xC7 #x23 #xC3 #x18 #x96 #x05 #x9A 
       #x07 #x12 #x80 #xE2 #xEB #x27 #xB2 #x75 
       #x09 #x83 #x2C #x1A #x1B #x6E #x5A #xA0 
       #x52 #x3B #xD6 #xB3 #x29 #xE3 #x2F #x84 
       #x53 #xD1 #x00 #xED #x20 #xFC #xB1 #x5B 
       #x6A #xCB #xBE #x39 #x4A #x4C #x58 #xCF 
       #xD0 #xEF #xAA #xFB #x43 #x4D #x33 #x85 
       #x45 #xF9 #x02 #x7F #x50 #x3C #x9F #xA8 
       #x51 #xA3 #x40 #x8F #x92 #x9D #x38 #xF5 
       #xBC #xB6 #xDA #x21 #x10 #xFF #xF3 #xD2 
       #xCD #x0C #x13 #xEC #x5F #x97 #x44 #x17 
       #xC4 #xA7 #x7E #x3D #x64 #x5D #x19 #x73 
       #x60 #x81 #x4F #xDC #x22 #x2A #x90 #x88 
       #x46 #xEE #xB8 #x14 #xDE #x5E #x0B #xDB 
       #xE0 #x32 #x3A #x0A #x49 #x06 #x24 #x5C 
       #xC2 #xD3 #xAC #x62 #x91 #x95 #xE4 #x79 
       #xE7 #xC8 #x37 #x6D #x8D #xD5 #x4E #xA9 
       #x6C #x56 #xF4 #xEA #x65 #x7A #xAE #x08 
       #xBA #x78 #x25 #x2E #x1C #xA6 #xB4 #xC6 
       #xE8 #xDD #x74 #x1F #x4B #xBD #x8B #x8A 
       #x70 #x3E #xB5 #x66 #x48 #x03 #xF6 #x0E 
       #x61 #x35 #x57 #xB9 #x86 #xC1 #x1D #x9E 
       #xE1 #xF8 #x98 #x11 #x69 #xD9 #x8E #x94 
       #x9B #x1E #x87 #xE9 #xCE #x55 #x28 #xDF 
       #x8C #xA1 #x89 #x0D #xBF #xE6 #x42 #x68 
       #x41 #x99 #x2D #x0F #xB0 #x54 #xBB #x16)))

(define-constant +rsb+
    (make-array
     256 :element-type 'uint-8
     :initial-contents
     '(#x52 #x09 #x6A #xD5 #x30 #x36 #xA5 #x38 
       #xBF #x40 #xA3 #x9E #x81 #xF3 #xD7 #xFB 
       #x7C #xE3 #x39 #x82 #x9B #x2F #xFF #x87 
       #x34 #x8E #x43 #x44 #xC4 #xDE #xE9 #xCB 
       #x54 #x7B #x94 #x32 #xA6 #xC2 #x23 #x3D 
       #xEE #x4C #x95 #x0B #x42 #xFA #xC3 #x4E 
       #x08 #x2E #xA1 #x66 #x28 #xD9 #x24 #xB2 
       #x76 #x5B #xA2 #x49 #x6D #x8B #xD1 #x25 
       #x72 #xF8 #xF6 #x64 #x86 #x68 #x98 #x16 
       #xD4 #xA4 #x5C #xCC #x5D #x65 #xB6 #x92 
       #x6C #x70 #x48 #x50 #xFD #xED #xB9 #xDA 
       #x5E #x15 #x46 #x57 #xA7 #x8D #x9D #x84 
       #x90 #xD8 #xAB #x00 #x8C #xBC #xD3 #x0A 
       #xF7 #xE4 #x58 #x05 #xB8 #xB3 #x45 #x06 
       #xD0 #x2C #x1E #x8F #xCA #x3F #x0F #x02 
       #xC1 #xAF #xBD #x03 #x01 #x13 #x8A #x6B 
       #x3A #x91 #x11 #x41 #x4F #x67 #xDC #xEA 
       #x97 #xF2 #xCF #xCE #xF0 #xB4 #xE6 #x73 
       #x96 #xAC #x74 #x22 #xE7 #xAD #x35 #x85 
       #xE2 #xF9 #x37 #xE8 #x1C #x75 #xDF #x6E 
       #x47 #xF1 #x1A #x71 #x1D #x29 #xC5 #x89 
       #x6F #xB7 #x62 #x0E #xAA #x18 #xBE #x1B 
       #xFC #x56 #x3E #x4B #xC6 #xD2 #x79 #x20 
       #x9A #xDB #xC0 #xFE #x78 #xCD #x5A #xF4 
       #x1F #xDD #xA8 #x33 #x88 #x07 #xC7 #x31 
       #xB1 #x12 #x10 #x59 #x27 #x80 #xEC #x5F 
       #x60 #x51 #x7F #xA9 #x19 #xB5 #x4A #x0D 
       #x2D #xE5 #x7A #x9F #x93 #xC9 #x9C #xEF 
       #xA0 #xE0 #x3B #x4D #xAE #x2A #xF5 #xB0 
       #xC8 #xEB #xBB #x3C #x83 #x53 #x99 #x61 
       #x17 #x2B #x04 #x7E #xBA #x77 #xD6 #x26 
       #xE1 #x69 #x14 #x63 #x55 #x21 #x0C #x7D)))

(define-constant +ft0+
    (make-array
     512 :element-type 'uint-16
     :initial-contents
     '(#xC663 #x63A5 #xF87C #x7C84 #xEE77 #x7799 #xF67B #x7B8D 
       #xFFF2 #xF20D #xD66B #x6BBD #xDE6F #x6FB1 #x91C5 #xC554 
       #x6030 #x3050 #x0201 #x0103 #xCE67 #x67A9 #x562B #x2B7D 
       #xE7FE #xFE19 #xB5D7 #xD762 #x4DAB #xABE6 #xEC76 #x769A 
       #x8FCA #xCA45 #x1F82 #x829D #x89C9 #xC940 #xFA7D #x7D87 
       #xEFFA #xFA15 #xB259 #x59EB #x8E47 #x47C9 #xFBF0 #xF00B 
       #x41AD #xADEC #xB3D4 #xD467 #x5FA2 #xA2FD #x45AF #xAFEA 
       #x239C #x9CBF #x53A4 #xA4F7 #xE472 #x7296 #x9BC0 #xC05B 
       #x75B7 #xB7C2 #xE1FD #xFD1C #x3D93 #x93AE #x4C26 #x266A 
       #x6C36 #x365A #x7E3F #x3F41 #xF5F7 #xF702 #x83CC #xCC4F 
       #x6834 #x345C #x51A5 #xA5F4 #xD1E5 #xE534 #xF9F1 #xF108 
       #xE271 #x7193 #xABD8 #xD873 #x6231 #x3153 #x2A15 #x153F 
       #x0804 #x040C #x95C7 #xC752 #x4623 #x2365 #x9DC3 #xC35E 
       #x3018 #x1828 #x3796 #x96A1 #x0A05 #x050F #x2F9A #x9AB5 
       #x0E07 #x0709 #x2412 #x1236 #x1B80 #x809B #xDFE2 #xE23D 
       #xCDEB #xEB26 #x4E27 #x2769 #x7FB2 #xB2CD #xEA75 #x759F 
       #x1209 #x091B #x1D83 #x839E #x582C #x2C74 #x341A #x1A2E 
       #x361B #x1B2D #xDC6E #x6EB2 #xB45A #x5AEE #x5BA0 #xA0FB 
       #xA452 #x52F6 #x763B #x3B4D #xB7D6 #xD661 #x7DB3 #xB3CE 
       #x5229 #x297B #xDDE3 #xE33E #x5E2F #x2F71 #x1384 #x8497 
       #xA653 #x53F5 #xB9D1 #xD168 #x0000 #x0000 #xC1ED #xED2C 
       #x4020 #x2060 #xE3FC #xFC1F #x79B1 #xB1C8 #xB65B #x5BED 
       #xD46A #x6ABE #x8DCB #xCB46 #x67BE #xBED9 #x7239 #x394B 
       #x944A #x4ADE #x984C #x4CD4 #xB058 #x58E8 #x85CF #xCF4A 
       #xBBD0 #xD06B #xC5EF #xEF2A #x4FAA #xAAE5 #xEDFB #xFB16 
       #x8643 #x43C5 #x9A4D #x4DD7 #x6633 #x3355 #x1185 #x8594 
       #x8A45 #x45CF #xE9F9 #xF910 #x0402 #x0206 #xFE7F #x7F81 
       #xA050 #x50F0 #x783C #x3C44 #x259F #x9FBA #x4BA8 #xA8E3 
       #xA251 #x51F3 #x5DA3 #xA3FE #x8040 #x40C0 #x058F #x8F8A 
       #x3F92 #x92AD #x219D #x9DBC #x7038 #x3848 #xF1F5 #xF504 
       #x63BC #xBCDF #x77B6 #xB6C1 #xAFDA #xDA75 #x4221 #x2163 
       #x2010 #x1030 #xE5FF #xFF1A #xFDF3 #xF30E #xBFD2 #xD26D 
       #x81CD #xCD4C #x180C #x0C14 #x2613 #x1335 #xC3EC #xEC2F 
       #xBE5F #x5FE1 #x3597 #x97A2 #x8844 #x44CC #x2E17 #x1739 
       #x93C4 #xC457 #x55A7 #xA7F2 #xFC7E #x7E82 #x7A3D #x3D47 
       #xC864 #x64AC #xBA5D #x5DE7 #x3219 #x192B #xE673 #x7395 
       #xC060 #x60A0 #x1981 #x8198 #x9E4F #x4FD1 #xA3DC #xDC7F 
       #x4422 #x2266 #x542A #x2A7E #x3B90 #x90AB #x0B88 #x8883 
       #x8C46 #x46CA #xC7EE #xEE29 #x6BB8 #xB8D3 #x2814 #x143C 
       #xA7DE #xDE79 #xBC5E #x5EE2 #x160B #x0B1D #xADDB #xDB76 
       #xDBE0 #xE03B #x6432 #x3256 #x743A #x3A4E #x140A #x0A1E 
       #x9249 #x49DB #x0C06 #x060A #x4824 #x246C #xB85C #x5CE4 
       #x9FC2 #xC25D #xBDD3 #xD36E #x43AC #xACEF #xC462 #x62A6 
       #x3991 #x91A8 #x3195 #x95A4 #xD3E4 #xE437 #xF279 #x798B 
       #xD5E7 #xE732 #x8BC8 #xC843 #x6E37 #x3759 #xDA6D #x6DB7 
       #x018D #x8D8C #xB1D5 #xD564 #x9C4E #x4ED2 #x49A9 #xA9E0 
       #xD86C #x6CB4 #xAC56 #x56FA #xF3F4 #xF407 #xCFEA #xEA25 
       #xCA65 #x65AF #xF47A #x7A8E #x47AE #xAEE9 #x1008 #x0818 
       #x6FBA #xBAD5 #xF078 #x7888 #x4A25 #x256F #x5C2E #x2E72 
       #x381C #x1C24 #x57A6 #xA6F1 #x73B4 #xB4C7 #x97C6 #xC651 
       #xCBE8 #xE823 #xA1DD #xDD7C #xE874 #x749C #x3E1F #x1F21 
       #x964B #x4BDD #x61BD #xBDDC #x0D8B #x8B86 #x0F8A #x8A85 
       #xE070 #x7090 #x7C3E #x3E42 #x71B5 #xB5C4 #xCC66 #x66AA 
       #x9048 #x48D8 #x0603 #x0305 #xF7F6 #xF601 #x1C0E #x0E12 
       #xC261 #x61A3 #x6A35 #x355F #xAE57 #x57F9 #x69B9 #xB9D0 
       #x1786 #x8691 #x99C1 #xC158 #x3A1D #x1D27 #x279E #x9EB9 
       #xD9E1 #xE138 #xEBF8 #xF813 #x2B98 #x98B3 #x2211 #x1133 
       #xD269 #x69BB #xA9D9 #xD970 #x078E #x8E89 #x3394 #x94A7 
       #x2D9B #x9BB6 #x3C1E #x1E22 #x1587 #x8792 #xC9E9 #xE920 
       #x87CE #xCE49 #xAA55 #x55FF #x5028 #x2878 #xA5DF #xDF7A 
       #x038C #x8C8F #x59A1 #xA1F8 #x0989 #x8980 #x1A0D #x0D17 
       #x65BF #xBFDA #xD7E6 #xE631 #x8442 #x42C6 #xD068 #x68B8 
       #x8241 #x41C3 #x2999 #x99B0 #x5A2D #x2D77 #x1E0F #x0F11 
       #x7BB0 #xB0CB #xA854 #x54FC #x6DBB #xBBD6 #x2C16 #x163A)))


(define-constant +rt0+
    (make-array
     512 :element-type 'uint-16
     :initial-contents
     '(#x51F4 #xA750 #x7E41 #x6553 #x1A17 #xA4C3 #x3A27 #x5E96 
       #x3BAB #x6BCB #x1F9D #x45F1 #xACFA #x58AB #x4BE3 #x0393 
       #x2030 #xFA55 #xAD76 #x6DF6 #x88CC #x7691 #xF502 #x4C25 
       #x4FE5 #xD7FC #xC52A #xCBD7 #x2635 #x4480 #xB562 #xA38F 
       #xDEB1 #x5A49 #x25BA #x1B67 #x45EA #x0E98 #x5DFE #xC0E1 
       #xC32F #x7502 #x814C #xF012 #x8D46 #x97A3 #x6BD3 #xF9C6 
       #x038F #x5FE7 #x1592 #x9C95 #xBF6D #x7AEB #x9552 #x59DA 
       #xD4BE #x832D #x5874 #x21D3 #x49E0 #x6929 #x8EC9 #xC844 
       #x75C2 #x896A #xF48E #x7978 #x9958 #x3E6B #x27B9 #x71DD 
       #xBEE1 #x4FB6 #xF088 #xAD17 #xC920 #xAC66 #x7DCE #x3AB4 
       #x63DF #x4A18 #xE51A #x3182 #x9751 #x3360 #x6253 #x7F45 
       #xB164 #x77E0 #xBB6B #xAE84 #xFE81 #xA01C #xF908 #x2B94 
       #x7048 #x6858 #x8F45 #xFD19 #x94DE #x6C87 #x527B #xF8B7 
       #xAB73 #xD323 #x724B #x02E2 #xE31F #x8F57 #x6655 #xAB2A 
       #xB2EB #x2807 #x2FB5 #xC203 #x86C5 #x7B9A #xD337 #x08A5 
       #x3028 #x87F2 #x23BF #xA5B2 #x0203 #x6ABA #xED16 #x825C 
       #x8ACF #x1C2B #xA779 #xB492 #xF307 #xF2F0 #x4E69 #xE2A1 
       #x65DA #xF4CD #x0605 #xBED5 #xD134 #x621F #xC4A6 #xFE8A 
       #x342E #x539D #xA2F3 #x55A0 #x058A #xE132 #xA4F6 #xEB75 
       #x0B83 #xEC39 #x4060 #xEFAA #x5E71 #x9F06 #xBD6E #x1051 
       #x3E21 #x8AF9 #x96DD #x063D #xDD3E #x05AE #x4DE6 #xBD46 
       #x9154 #x8DB5 #x71C4 #x5D05 #x0406 #xD46F #x6050 #x15FF 
       #x1998 #xFB24 #xD6BD #xE997 #x8940 #x43CC #x67D9 #x9E77 
       #xB0E8 #x42BD #x0789 #x8B88 #xE719 #x5B38 #x79C8 #xEEDB 
       #xA17C #x0A47 #x7C42 #x0FE9 #xF884 #x1EC9 #x0000 #x0000 
       #x0980 #x8683 #x322B #xED48 #x1E11 #x70AC #x6C5A #x724E 
       #xFD0E #xFFFB #x0F85 #x3856 #x3DAE #xD51E #x362D #x3927 
       #x0A0F #xD964 #x685C #xA621 #x9B5B #x54D1 #x2436 #x2E3A 
       #x0C0A #x67B1 #x9357 #xE70F #xB4EE #x96D2 #x1B9B #x919E 
       #x80C0 #xC54F #x61DC #x20A2 #x5A77 #x4B69 #x1C12 #x1A16 
       #xE293 #xBA0A #xC0A0 #x2AE5 #x3C22 #xE043 #x121B #x171D 
       #x0E09 #x0D0B #xF28B #xC7AD #x2DB6 #xA8B9 #x141E #xA9C8 
       #x57F1 #x1985 #xAF75 #x074C #xEE99 #xDDBB #xA37F #x60FD 
       #xF701 #x269F #x5C72 #xF5BC #x4466 #x3BC5 #x5BFB #x7E34 
       #x8B43 #x2976 #xCB23 #xC6DC #xB6ED #xFC68 #xB8E4 #xF163 
       #xD731 #xDCCA #x4263 #x8510 #x1397 #x2240 #x84C6 #x1120 
       #x854A #x247D #xD2BB #x3DF8 #xAEF9 #x3211 #xC729 #xA16D 
       #x1D9E #x2F4B #xDCB2 #x30F3 #x0D86 #x52EC #x77C1 #xE3D0 
       #x2BB3 #x166C #xA970 #xB999 #x1194 #x48FA #x47E9 #x6422 
       #xA8FC #x8CC4 #xA0F0 #x3F1A #x567D #x2CD8 #x2233 #x90EF 
       #x8749 #x4EC7 #xD938 #xD1C1 #x8CCA #xA2FE #x98D4 #x0B36 
       #xA6F5 #x81CF #xA57A #xDE28 #xDAB7 #x8E26 #x3FAD #xBFA4 
       #x2C3A #x9DE4 #x5078 #x920D #x6A5F #xCC9B #x547E #x4662 
       #xF68D #x13C2 #x90D8 #xB8E8 #x2E39 #xF75E #x82C3 #xAFF5 
       #x9F5D #x80BE #x69D0 #x937C #x6FD5 #x2DA9 #xCF25 #x12B3 
       #xC8AC #x993B #x1018 #x7DA7 #xE89C #x636E #xDB3B #xBB7B 
       #xCD26 #x7809 #x6E59 #x18F4 #xEC9A #xB701 #x834F #x9AA8 
       #xE695 #x6E65 #xAAFF #xE67E #x21BC #xCF08 #xEF15 #xE8E6 
       #xBAE7 #x9BD9 #x4A6F #x36CE #xEA9F #x09D4 #x29B0 #x7CD6 
       #x31A4 #xB2AF #x2A3F #x2331 #xC6A5 #x9430 #x35A2 #x66C0 
       #x744E #xBC37 #xFC82 #xCAA6 #xE090 #xD0B0 #x33A7 #xD815 
       #xF104 #x984A #x41EC #xDAF7 #x7FCD #x500E #x1791 #xF62F 
       #x764D #xD68D #x43EF #xB04D #xCCAA #x4D54 #xE496 #x04DF 
       #x9ED1 #xB5E3 #x4C6A #x881B #xC12C #x1FB8 #x4665 #x517F 
       #x9D5E #xEA04 #x018C #x355D #xFA87 #x7473 #xFB0B #x412E 
       #xB367 #x1D5A #x92DB #xD252 #xE910 #x5633 #x6DD6 #x4713 
       #x9AD7 #x618C #x37A1 #x0C7A #x59F8 #x148E #xEB13 #x3C89 
       #xCEA9 #x27EE #xB761 #xC935 #xE11C #xE5ED #x7A47 #xB13C 
       #x9CD2 #xDF59 #x55F2 #x733F #x1814 #xCE79 #x73C7 #x37BF 
       #x53F7 #xCDEA #x5FFD #xAA5B #xDF3D #x6F14 #x7844 #xDB86 
       #xCAAF #xF381 #xB968 #xC43E #x3824 #x342C #xC2A3 #x405F 
       #x161D #xC372 #xBCE2 #x250C #x283C #x498B #xFF0D #x9541 
       #x39A8 #x0171 #x080C #xB3DE #xD8B4 #xE49C #x6456 #xC190 
       #x7BCB #x8461 #xD532 #xB670 #x486C #x5C74 #xD0B8 #x5742)))

;; Generate the remaining forwad and reverse tables based on
;; the above
(define-constant +ft1+ (array-rotate-uint-16-pairs-right +ft0+))
(define-constant +ft2+ (array-rotate-uint-16-pairs-right +ft1+))
(define-constant +ft3+ (array-rotate-uint-16-pairs-right +ft2+))
(define-constant +rt1+ (array-rotate-uint-16-pairs-right +rt0+))
(define-constant +rt2+ (array-rotate-uint-16-pairs-right +rt1+))
(define-constant +rt3+ (array-rotate-uint-16-pairs-right +rt2+))

;; Reverse key expansion tables
(define-constant +kt0+ (gen-k-table +rt0+))
(define-constant +kt1+ (gen-k-table +rt1+))
(define-constant +kt2+ (gen-k-table +rt2+))
(define-constant +kt3+ (gen-k-table +rt3+))
	 
	  

       
;;;
;;; AES Support Routines
;;;


(defmacro make-uint-16 (b1 b0)
  ;; A macro to avoid varying treatment of inline across Lisps
  `(@logxor (ash ,b1 8) ,b0))

(defmacro make-bytes-from-uint-16 (w array offset)
  ;; A macro to avoid varying treatment of inline across Lisps
  `(progn
     (setf (@aref uint-8 ,array ,offset) (ldb (byte 8 8) ,w))
     (setf (@aref uint-8 ,array (1+ ,offset)) (ldb (byte 8 0) ,w))))
     
(defmacro make-uint-16-from-byte-array (array offset)
  "Combine adjacent bytes at offset into a uint-16"
  ;;This is a macro to avoid run-time index arithmetic
  `(make-uint-16 (aref ,array ,offset)
		 (aref ,array ,(1+ offset))))
  

(defmacro fill-byte-array-from-uint-16s (words array)
  "Fill in a byte-array from uint-16s"
  ;;This is a macro to avoid run-time index arithmetic
  (let* ((out-ix -2)
	 (form
	  (mapcar #'(lambda (w)
		      `(make-bytes-from-uint-16 ,w ,array ,(incf out-ix 2)))
		  words)))
    `(progn
       ,@form
       ,array)))

(defun make-uint-32 (b3 b2 b1 b0)
  ;; can't guaranteed not bignum on 32-bit CCL
  ;; (declare (optimize (speed 3) (safety 0))
  ;; 	   (type (unsigned-byte 8) b3 b2 b1 b0))
  (logxor (ash b3 24)
	  (ash b2 16)
	  (ash b1 8)
	  b0))

(defun sub-uint-32 (w)
  (make-uint-32 (aref +fsb+ (ldb (byte 8 24) w))
		(aref +fsb+ (ldb (byte 8 16) w))
		(aref +fsb+ (ldb (byte 8 8) w))
		(aref +fsb+ (logand #xFF w))))

(defun make-uint-32-from-byte-array (byte-array offset)
  ;; can't guarantee not bignum on 32-bit CCL
  ;; (declare (optimize (speed 3) (safety 0))
  ;; 	   (type fixnum offset)
  ;; 	   (type (simple-array uint-8) byte-array))
  (make-uint-32 (aref byte-array offset)
	       (aref byte-array (@+ 1 offset))
 	       (aref byte-array (@+ 2 offset))
	       (aref byte-array (@+ 3 offset))))

       

;;;
;;; AES Key Expansion
;;;

;; For now, key expansion uses 32-bit operations
;; with the resulting key schedules converted
;; to uint-16 arrays.

(defun uint-32-array->uint-16-array (in)
  (let* ((num-words (array-total-size in))
	 (out (make-array (* 2 num-words)
			  :element-type '(uint-16))))
    (do ((i 0 (1+ i))
	 (j 0 (+ 2 j)))
	((= i num-words))
      (setf (aref out j)
	    (ldb (byte 16 16) (the uint-32 (aref in i))))
      (setf (aref out (1+ j))
	    (ldb (byte 16 0) (aref in i))))
    out))
	
(defun prepare-reverse-key (fkey num-rounds)
  (declare (type fixnum num-rounds))
  (let* ((num-keys (array-total-size fkey))
	 (rkey (make-array num-keys)))
    (declare (type fixnum num-keys))
    ; First and last rounds do not use mix-columns
    (dotimes (i 4)
      (declare (type fixnum i))
      (setf (aref rkey i) (aref fkey (@+ i (@- num-keys 4))))
      (setf (aref rkey (@+ i (@- num-keys 4))) (aref fkey i)))
    (dotimes (r (@- num-rounds 1))
      (declare (type fixnum r))
      (let ((f-ix (- num-keys (@* 4 (@+ 2 r))))
	    (r-ix (@* 4 (@+ 1 r))))
	(dotimes (i 4)
	  (declare (type fixnum i))
	  (let ((tmp (aref fkey (@+ f-ix i))))
	    (setf (aref rkey (@+ r-ix i))
		  (logxor (aref +kt0+ (ldb (byte 8 24) tmp))
			  (aref +kt1+ (ldb (byte 8 16) tmp))
			  (aref +kt2+ (ldb (byte 8 8) tmp))
			  (aref +kt3+ (ldb (byte 8 0) tmp))))))))
    rkey))

(defun aes-expand-key (raw-key)
  (declare (type (simple-array uint-8) raw-key))
  (unless (member (array-total-size raw-key) '(16 24 32))
    (error "Invalid key size.  Must be 16, 24, or 32 bytes."))
  (let* ((num-words (/ (array-total-size raw-key) 4))
	 (num-rounds (case num-words (4 10) (6 12) (8 14)))
	 (fkey (make-array (* +block-words+ (1+ num-rounds)))))
    (dotimes (i num-words)
      (setf (aref fkey i) (make-uint-32-from-byte-array raw-key (* i 4))))
    (for (i num-words (1- (* +block-words+ (1+ num-rounds))))
      (setf (aref fkey i)
	    (logxor (aref fkey (- i num-words))
  		    (cond ((zerop (mod i num-words))
  			   (logxor (nth (1- (/ i num-words)) +rcon+)
  				   (sub-uint-32 (rot-uint-32-L
  					      (aref fkey (1- i))))))
  			  ((and (> num-words 6)
  				(= 4 (mod i num-words)))
  			   (sub-uint-32 (aref fkey (1- i))))
  			  (t
  			   (aref fkey (1- i)))))))
    (make-instance 'aes-key
		   :num-rounds num-rounds
		   :forward-key (uint-32-array->uint-16-array fkey)
		   :reverse-key (uint-32-array->uint-16-array
				 (prepare-reverse-key fkey num-rounds)))))




;;;
;;; AES encrypt/decrypt routines
;;;

(defmacro aes-round-step (t0 t1 t2 t3 rkh rkl b0 b1 b2 b3 xh xl)
  (with-gensyms (g0 g1 g2 g3)
    `(let ((,g0 (@* 2 ,b0))
	   (,g1 (@* 2 ,b1))
	   (,g2 (@* 2 ,b2))
	   (,g3 (@* 2 ,b3)))
       (setq ,xh (@logxor ,rkh
			  (aref ,t0 ,g0)
			  (aref ,t1 ,g1)
			  (aref ,t2 ,g2)
			  (aref ,t3 ,g3)))
       (setq ,xl (@logxor ,rkl
			  (aref ,t0 (@+ 1 ,g0))
			  (aref ,t1 (@+ 1 ,g1))
			  (aref ,t2 (@+ 1 ,g2))
			  (aref ,t3 (@+ 1 ,g3)))))))


(defmacro aes-last-round-step (sb rkh rkl b0 b1 b2 b3 xh xl)
  `(progn
     (setq ,xh (@logxor ,rkh
		       (@ash (aref ,sb ,b0) 8)
		       (aref ,sb ,b1)))
     (setq ,xl (@logxor ,rkl
		       (@ash (aref ,sb ,b2) 8)
		       (aref ,sb ,b3)))))


(defmacro aes-f-round (round-keys rk-ix
		       w0-hi w0-lo w1-hi w1-lo w2-hi w2-lo w3-hi w3-lo
		       x0-hi x0-lo x1-hi x1-lo x2-hi x2-lo x3-hi x3-lo)
  `(progn
     (aes-round-step +ft0+ +ft1+ +ft2+ +ft3+
		     (aref ,round-keys ,rk-ix)
		     (aref ,round-keys (@+ 1 ,rk-ix))
		     (ldb (byte 8 8) ,w0-hi)
		     (ldb (byte 8 0) ,w1-hi)
		     (ldb (byte 8 8) ,w2-lo)
		     (ldb (byte 8 0) ,w3-lo)
		     ,x0-hi ,x0-lo)
     (aes-round-step +ft0+ +ft1+ +ft2+ +ft3+
		     (aref ,round-keys (@+ 2 ,rk-ix))
		     (aref ,round-keys (@+ 3 ,rk-ix))
		     (ldb (byte 8 8) ,w1-hi)
		     (ldb (byte 8 0) ,w2-hi)
		     (ldb (byte 8 8) ,w3-lo)
		     (ldb (byte 8 0) ,w0-lo)
		     ,x1-hi ,x1-lo)
     (aes-round-step +ft0+ +ft1+ +ft2+ +ft3+
		     (aref ,round-keys (@+ 4 ,rk-ix))
		     (aref ,round-keys (@+ 5 ,rk-ix))
		     (ldb (byte 8 8) ,w2-hi)
		     (ldb (byte 8 0) ,w3-hi)
		     (ldb (byte 8 8) ,w0-lo)
		     (ldb (byte 8 0) ,w1-lo)
		     ,x2-hi ,x2-lo)
     (aes-round-step +ft0+ +ft1+ +ft2+ +ft3+
		     (aref ,round-keys (@+ 6 ,rk-ix))
		     (aref ,round-keys (@+ 7 ,rk-ix))
		     (ldb (byte 8 8) ,w3-hi)
		     (ldb (byte 8 0) ,w0-hi)
		     (ldb (byte 8 8) ,w1-lo)
		     (ldb (byte 8 0) ,w2-lo)
		     ,x3-hi ,x3-lo)))
     
(defmacro aes-last-f-round (round-keys rk-ix
			    w0-hi w0-lo w1-hi w1-lo w2-hi w2-lo w3-hi w3-lo
			    x0-hi x0-lo x1-hi x1-lo x2-hi x2-lo x3-hi x3-lo)
  `(progn
     (aes-last-round-step +fsb+
			  (aref ,round-keys ,rk-ix)
			  (aref ,round-keys (@+ 1 ,rk-ix))
			  (ldb (byte 8 8) ,w0-hi)
			  (ldb (byte 8 0) ,w1-hi)
			  (ldb (byte 8 8) ,w2-lo)
			  (ldb (byte 8 0) ,w3-lo)
			  ,x0-hi ,x0-lo)
     (aes-last-round-step +fsb+
			  (aref ,round-keys (@+ 2 ,rk-ix))
			  (aref ,round-keys (@+ 3 ,rk-ix))
			  (ldb (byte 8 8) ,w1-hi)
			  (ldb (byte 8 0) ,w2-hi)
			  (ldb (byte 8 8) ,w3-lo)
			  (ldb (byte 8 0) ,w0-lo)
			  ,x1-hi ,x1-lo)
     (aes-last-round-step +fsb+
			  (aref ,round-keys (@+ 4 ,rk-ix))
			  (aref ,round-keys (@+ 5 ,rk-ix))
			  (ldb (byte 8 8) ,w2-hi)
			  (ldb (byte 8 0) ,w3-hi)
			  (ldb (byte 8 8) ,w0-lo)
			  (ldb (byte 8 0) ,w1-lo)
			  ,x2-hi ,x2-lo)
     (aes-last-round-step +fsb+
			  (aref ,round-keys (@+ 6 ,rk-ix))
			  (aref ,round-keys (@+ 7 ,rk-ix))
			  (ldb (byte 8 8) ,w3-hi)
			  (ldb (byte 8 0) ,w0-hi)
			  (ldb (byte 8 8) ,w1-lo)
			  (ldb (byte 8 0) ,w2-lo)
			  ,x3-hi ,x3-lo)))

;; Reverse

(defmacro aes-r-round (round-keys rk-ix
		       w0-hi w0-lo w1-hi w1-lo w2-hi w2-lo w3-hi w3-lo
		       x0-hi x0-lo x1-hi x1-lo x2-hi x2-lo x3-hi x3-lo)
  `(progn
     (aes-round-step +rt0+ +rt1+ +rt2+ +rt3+
		     (aref ,round-keys ,rk-ix)
		     (aref ,round-keys (@+ 1 ,rk-ix))
		     (ldb (byte 8 8) ,w0-hi)
		     (ldb (byte 8 0) ,w3-hi)
		     (ldb (byte 8 8) ,w2-lo)
		     (ldb (byte 8 0) ,w1-lo)
		     ,x0-hi ,x0-lo)
     (aes-round-step +rt0+ +rt1+ +rt2+ +rt3+
		     (aref ,round-keys (@+ 2 ,rk-ix))
		     (aref ,round-keys (@+ 3 ,rk-ix))
		     (ldb (byte 8 8) ,w1-hi)
		     (ldb (byte 8 0) ,w0-hi)
		     (ldb (byte 8 8) ,w3-lo)
		     (ldb (byte 8 0) ,w2-lo)
		     ,x1-hi ,x1-lo)
     (aes-round-step +rt0+ +rt1+ +rt2+ +rt3+
		     (aref ,round-keys (@+ 4 ,rk-ix))
		     (aref ,round-keys (@+ 5 ,rk-ix))
		     (ldb (byte 8 8) ,w2-hi)
		     (ldb (byte 8 0) ,w1-hi)
		     (ldb (byte 8 8) ,w0-lo)
		     (ldb (byte 8 0) ,w3-lo)
		     ,x2-hi ,x2-lo)
     (aes-round-step +rt0+ +rt1+ +rt2+ +rt3+
		     (aref ,round-keys (@+ 6 ,rk-ix))
		     (aref ,round-keys (@+ 7 ,rk-ix))
		     (ldb (byte 8 8) ,w3-hi)
		     (ldb (byte 8 0) ,w2-hi)
		     (ldb (byte 8 8) ,w1-lo)
		     (ldb (byte 8 0) ,w0-lo)
		     ,x3-hi ,x3-lo)))
     
(defmacro aes-last-r-round (round-keys rk-ix
			    w0-hi w0-lo w1-hi w1-lo w2-hi w2-lo w3-hi w3-lo
			    x0-hi x0-lo x1-hi x1-lo x2-hi x2-lo x3-hi x3-lo)
  `(progn
     (aes-last-round-step +rsb+
			  (aref ,round-keys ,rk-ix)
			  (aref ,round-keys (@+ 1 ,rk-ix))
			  (ldb (byte 8 8) ,w0-hi)
			  (ldb (byte 8 0) ,w3-hi)
			  (ldb (byte 8 8) ,w2-lo)
			  (ldb (byte 8 0) ,w1-lo)
			  ,x0-hi ,x0-lo)
     (aes-last-round-step +rsb+
			  (aref ,round-keys (@+ 2 ,rk-ix))
			  (aref ,round-keys (@+ 3 ,rk-ix))
			  (ldb (byte 8 8) ,w1-hi)
			  (ldb (byte 8 0) ,w0-hi)
			  (ldb (byte 8 8) ,w3-lo)
			  (ldb (byte 8 0) ,w2-lo)
			  ,x1-hi ,x1-lo)
     (aes-last-round-step +rsb+
			  (aref ,round-keys (@+ 4 ,rk-ix))
			  (aref ,round-keys (@+ 5 ,rk-ix))
			  (ldb (byte 8 8) ,w2-hi)
			  (ldb (byte 8 0) ,w1-hi)
			  (ldb (byte 8 8) ,w0-lo)
			  (ldb (byte 8 0) ,w3-lo)
			  ,x2-hi ,x2-lo)
     (aes-last-round-step +rsb+
			  (aref ,round-keys (@+ 6 ,rk-ix))
			  (aref ,round-keys (@+ 7 ,rk-ix))
			  (ldb (byte 8 8) ,w3-hi)
			  (ldb (byte 8 0) ,w2-hi)
			  (ldb (byte 8 8) ,w1-lo)
			  (ldb (byte 8 0) ,w0-lo)
			  ,x3-hi ,x3-lo)))



(defmacro with-init-aes-vars (in round-keys vars &body body)
  "Initialize the input uint-16s from round-keys"
  (let* ((in-ix -1)
	 (out-ix -2)
	 (form
	  (mapcar #'(lambda (v)
		      `(setq ,v
			     (@logxor
			      (aref ,round-keys ,(incf in-ix))
			      (make-uint-16-from-byte-array ,in ,(incf out-ix 2)))))
		  vars)))
    `(progn
       ,@form
       ,@body)))


(defun aes-encrypt (keys in &key (out (make-array 16 :element-type '(unsigned-byte 8))))
  (declare (optimize (speed 3) (safety 0))
	    (type (simple-array uint-8) in out))
  (let ((round-keys (forward-key-of keys)))
    (declare (type (simple-array uint-16) round-keys))
    (let ((w0-hi 0) (w1-hi 0) (w2-hi 0) (w3-hi 0)
	  (w0-lo 0) (w1-lo 0) (w2-lo 0) (w3-lo 0)	  
	  (x0-hi 0) (x1-hi 0) (x2-hi 0) (x3-hi 0)
	  (x0-lo 0) (x1-lo 0) (x2-lo 0) (x3-lo 0)
	  (num-rounds (num-rounds keys)))
      (declare (type fixnum
		     w0-hi w1-hi w2-hi w3-hi w0-lo w1-lo w2-lo w3-lo
		     x0-hi x1-hi x2-hi x3-hi x0-lo x1-lo x2-lo x3-lo
		     num-rounds))
      (with-init-aes-vars
	  in round-keys (w0-hi w0-lo w1-hi w1-lo w2-hi w2-lo w3-hi w3-lo)
	(do* ((i 1 (@+ i 2))
	      (rk-ix 8 (@+ 16 rk-ix)))
	     ((= i (@- num-rounds 1)))
	  (declare (type fixnum i))
	  (aes-f-round round-keys rk-ix
		       w0-hi w0-lo w1-hi w1-lo w2-hi w2-lo w3-hi w3-lo
		       x0-hi x0-lo x1-hi x1-lo x2-hi x2-lo x3-hi x3-lo)
	  (aes-f-round round-keys (@+ 8 rk-ix)
		       x0-hi x0-lo x1-hi x1-lo x2-hi x2-lo x3-hi x3-lo
		       w0-hi w0-lo w1-hi w1-lo w2-hi w2-lo w3-hi w3-lo))
	(aes-f-round round-keys (@* 8 (@- num-rounds 1))
		     w0-hi w0-lo w1-hi w1-lo w2-hi w2-lo w3-hi w3-lo
		     x0-hi x0-lo x1-hi x1-lo x2-hi x2-lo x3-hi x3-lo)
	(aes-last-f-round round-keys (@* 8 num-rounds)
			  x0-hi x0-lo x1-hi x1-lo x2-hi x2-lo x3-hi x3-lo
			  w0-hi w0-lo w1-hi w1-lo w2-hi w2-lo w3-hi w3-lo)
	(fill-byte-array-from-uint-16s (w0-hi w0-lo w1-hi w1-lo
					      w2-hi w2-lo w3-hi w3-lo)
				       out))))
  out)


(defun aes-decrypt (keys in &key (out (make-array 16 :element-type '(unsigned-byte 8))))
  (declare (optimize (speed 3) (safety 0))
	   (type (simple-array uint-8) in out))	   
  (let ((round-keys (reverse-key-of keys)))
    (declare (type (simple-array uint-16) round-keys))
    (let ((w0-hi 0) (w1-hi 0) (w2-hi 0) (w3-hi 0)
	  (w0-lo 0) (w1-lo 0) (w2-lo 0) (w3-lo 0)	  
	  (x0-hi 0) (x1-hi 0) (x2-hi 0) (x3-hi 0)
	  (x0-lo 0) (x1-lo 0) (x2-lo 0) (x3-lo 0)
	  (num-rounds (num-rounds keys)))
      (declare (type fixnum
		     w0-hi w1-hi w2-hi w3-hi w0-lo w1-lo w2-lo w3-lo
		     x0-hi x1-hi x2-hi x3-hi x0-lo x1-lo x2-lo x3-lo
		     num-rounds))
      (with-init-aes-vars
	  in round-keys (w0-hi w0-lo w1-hi w1-lo w2-hi w2-lo w3-hi w3-lo)
	(do* ((i 1 (@+ i 2))
	      (rk-ix 8 (@+ 16 rk-ix)))
	     ((= i (@- num-rounds 1)))
	  (declare (type fixnum i))
	  (aes-r-round round-keys rk-ix
		       w0-hi w0-lo w1-hi w1-lo w2-hi w2-lo w3-hi w3-lo
		       x0-hi x0-lo x1-hi x1-lo x2-hi x2-lo x3-hi x3-lo)
	  (aes-r-round round-keys (@+ 8 rk-ix)
		       x0-hi x0-lo x1-hi x1-lo x2-hi x2-lo x3-hi x3-lo
		       w0-hi w0-lo w1-hi w1-lo w2-hi w2-lo w3-hi w3-lo))
	(aes-r-round round-keys (@* 8 (@- num-rounds 1))
		     w0-hi w0-lo w1-hi w1-lo w2-hi w2-lo w3-hi w3-lo
		     x0-hi x0-lo x1-hi x1-lo x2-hi x2-lo x3-hi x3-lo)
	(aes-last-r-round round-keys (@* 8 num-rounds)
			  x0-hi x0-lo x1-hi x1-lo x2-hi x2-lo x3-hi x3-lo
			  w0-hi w0-lo w1-hi w1-lo w2-hi w2-lo w3-hi w3-lo)
	(fill-byte-array-from-uint-16s (w0-hi w0-lo w1-hi w1-lo
					      w2-hi w2-lo w3-hi w3-lo)
				     out))))
  out)


;;;
;;; Tests, borrowed from Joern Inge Vestgaarden's aes32 implementation
;;;

(defmacro with-timer (result &body body)
  (with-gensyms (start stop ret)
    `(let* ((,start (get-internal-run-time))
	    (,ret (progn ,@body))
	    (,stop (get-internal-run-time)))
       (setf ,result (/ (- ,stop ,start) internal-time-units-per-second 1.0))
       ,ret)))


(defun aes-get-speed (&key (bits 128) (times 100000) (direction :encrypt))
  "Return encrypted bits per second"
  (flet ((a (len) (make-array len
			      :initial-element 0		       
			      :element-type 'uint-8)))
    (let ((x (a 16))
	  (y (a 16))
	  (key (aes-expand-key (a (/ bits 8))))
	  (the-time))
      (if (eq direction :encrypt)
	  (with-timer the-time 
	    (dotimes (i times) 
	      (aes-encrypt key x :out y)))
	  (with-timer the-time 
	    (dotimes (i times) 
	      (aes-decrypt key x :out y))))
      (values (* bits times (/ the-time))
	      the-time))))

(defun aes-get-avg-speed (&key (iterations 10) (times 100000) (bits 128) (direction :encrypt))
  (let ((bit-sum 0)
	(time-sum 0))
    (dotimes (i iterations)
      (multiple-value-bind (bits time)
	  (aes-get-speed :bits bits :times times :direction direction)
	(incf bit-sum bits)
	(incf time-sum time)))
    (values
	    (/ bit-sum iterations)
	    (/ time-sum iterations))))

(defun hex-str->bin-array (hex-str)
  "Converts a hex string to binary array. Length of
hex string must be mulitple of 2"
  (let* ((bin-len (/ (length hex-str) 2))
	 (bin (make-array bin-len :element-type 'uint-8)))
    (dotimes (i bin-len)
      (setf (aref bin i)
	    (parse-integer hex-str :radix 16
			   :start (* 2 i)
			   :end (* 2 (1+ i)))))   
    bin))

(defparameter *aes-test-values*
  '((:pt "00112233445566778899aabbccddeeff"
     :key "000102030405060708090a0b0c0d0e0f"
     :ct "69c4e0d86a7b0430d8cdb78070b4c55a")
    (:pt "00112233445566778899aabbccddeeff"
     :key "000102030405060708090a0b0c0d0e0f1011121314151617"
     :ct "dda97ca4864cdfe06eaf70a0ec0d7191")
    (:pt "00112233445566778899aabbccddeeff"
     :key "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"
     :ct "8ea2b7ca516745bfeafc49904b496089")))

(defun aes-self-test (&key (data *aes-test-values*) (verbose t))
  "Short development test for consistency of aes"
  (labels ((cb (hex) (hex-str->bin-array hex))
	   (test (&key key pt ct)
	     (and (equalp (cb ct) 
			  (aes-encrypt (aes-expand-key (cb key)) (cb pt))) 
		  (equalp (cb pt) 
			  (aes-decrypt (aes-expand-key (cb key)) (cb ct)))))) 
    (mapcar (lambda (x) 
	      (let ((ok? (apply #'test x)))
		(when (and verbose (not ok?))
		  (format t "~&AES test fail for ~A~%" x))
		ok?))
	    data)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Copyright 2010 TSC AG, Postfach 73, CH 6314 Unterageri, Switzerland
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions
;;; and limitations under the License.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

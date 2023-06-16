  ;;; Nothing special about the "CFFI-USER" package.  We're just
  ;;; using it as a substitute for your own CL package.
(defpackage simplex
  (:use :common-lisp :cffi)
  (:export :simpx))

(in-package simplex)


(define-foreign-library libsimplex
    (:unix "../noise/libsimplex.so")
    (t (:default "libsimplex")))
(use-foreign-library libsimplex)
(defctype size :unsigned-int)

(defcfun "simplex" :float
    (x :float) (y :float)
    (seed size) (octaves size)
    (freq :float) (amp :float)
    (lacuna :float) (persistence :float))


(defun simpx (x y &optional seed octaves freq amp lac pers)
    (simplex
        (float x)
        (float y)
        (floor (or  seed 0))
        (floor (or  octaves 16))
        (float (or  freq 0.1))
        (float (or  amp 1.0))
        (float (or  lac 1.99))
        (float (or  pers 0.5))))



(define-foreign-library libspooky
    (:unix "../noise/libspooky.so")
    (t (:default "libspooky")))
(use-foreign-library libspooky)

(defcfun "spooky_64" :pointer
    (m :string) (length :size)
     (seed :uint64))

(defcfun "strlen" :size
    (m :string))

(foreign-string-to-lisp (foreign-funcall "echho"
                                :string "H"
                                :size 1
                                :pointer))

(defun get-spooky-hash (input seed)
    (foreign-string-to-lisp (foreign-funcall "spooky_128"
                                :string input
                                :size (length input)
                                :uint64 seed
                                :pointer)))
(defun get-spooky-hash-32 (input seed &optional lo)
     (foreign-funcall "spooky_32r"
         :string input
         :size (or lo (length input))
         :uint64 seed
         :uint32))

(get-spooky-hash-32 "he" 0 2)
(get-spooky-hash-32 "he" 0)


(defun get-random-spooky (input seed)
    (let ((hash (subseq (get-spooky-hash *lorem-text* 0) 0 2)))
        (/ (parse-integer hash :radix 16) 255)))

(defun hash-test (hash-fn num-buckets num-keys)
  (setq optimal-bucket-size (/ num-keys num-buckets))
  (let ((histogram (make-hash-table :test 'equal)))
    (dotimes (i num-keys)
      (let* ((key (format nil "key-~a" i))
             (bucket (mod (funcall hash-fn key) num-buckets)))
        (if (not (gethash bucket histogram))
            (setf (gethash bucket histogram) 1)
            (incf (gethash bucket histogram)))))
    (let ((expected-count (* num-keys (/ 1 num-buckets)))
          (sum-of-squares 0))
      (maphash (lambda (k v)
                 (let ((deviation (- v expected-count)))
                   (incf sum-of-squares
                          (/ (* deviation deviation) expected-count))))
               histogram)
      (/ sum-of-squares optimal-bucket-size))))


(defvar known32
    (list
              #x6bf50919 #x70de1d26 #xa2b37298 #x35bc5fbf #x8223b279 #x5bcb315e #x53fe88a1 #xf9f1a233
              #xee193982 #x54f86f29 #xc8772d36 #x9ed60886 #x5f23d1da #x1ed9f474 #xf2ef0c89 #x83ec01f9
              #xf274736c #x7e9ac0df #xc7aed250 #xb1015811 #xe23470f5 #x48ac20c4 #xe2ab3cd5 #x608f8363
              #xd0639e68 #xc4e8e7ab #x863c7c5b #x4ea63579 #x99ae8622 #x170c658b #x149ba493 #x027bca7c
              #xe5cfc8b6 #xce01d9d7 #x11103330 #x5d1f5ed4 #xca720ecb #xef408aec #x733b90ec #x855737a6
              #x9856c65f #x647411f7 #x50777c74 #xf0f1a8b7 #x9d7e55a5 #xc68dd371 #xfc1af2cc #x75728d0a
              #x390e5fdc #xf389b84c #xfb0ccf23 #xc95bad0e #x5b1cb85a #x6bdae14f #x6deb4626 #x93047034
              #x6f3266c6 #xf529c3bd #x396322e7 #x3777d042 #x1cd6a5a2 #x197b402e #xc28d0d2b #x09c1afb4

              #x069c8bb7 #x6f9d4e1e #xd2621b5c #xea68108d #x8660cb8f #xd61e6de6 #x7fba15c7 #xaacfaa97
              #xdb381902 #x4ea22649 #x5d414a1e #xc3fc5984 #xa0fc9e10 #x347dc51c #x37545fb6 #x8c84b26b
              #xf57efa5d #x56afaf16 #xb6e1eb94 #x9218536a #xe3cc4967 #xd3275ef4 #xea63536e #x6086e499
              #xaccadce7 #xb0290d82 #x4ebfd0d6 #x46ccc185 #x2eeb10d3 #x474e3c8c #x23c84aee #x3abae1cb
              #x1499b81a #xa2993951 #xeed176ad #xdfcfe84c #xde4a961f #x4af13fe6 #xe0069c42 #xc14de8f5
              #x6e02ce8f #x90d19f7f #xbca4a484 #xd4efdd63 #x780fd504 #xe80310e3 #x03abbc12 #x90023849
              #xd6f6fb84 #xd6b354c5 #x5b8575f0 #x758f14e4 #x450de862 #x90704afb #x47209a33 #xf226b726
              #xf858dab8 #x7c0d6de9 #xb05ce777 #xee5ff2d4 #x7acb6d5c #x2d663f85 #x41c72a91 #x82356bf2

              #x94e948ec #xd358d448 #xeca7814d #x78cd7950 #xd6097277 #x97782a5d #xf43fc6f4 #x105f0a38
              #x9e170082 #x4bfe566b #x4371d25f #xef25a364 #x698eb672 #x74f850e4 #x4678ff99 #x4a290dc6
              #x3918f07c #x32c7d9cd #x9f28e0af #x0d3c5a86 #x7bfc8a45 #xddf0c7e1 #xdeacb86b #x970b3c5c
              #x5e29e199 #xea28346d #x6b59e71b #xf8a8a46a #x862f6ce4 #x3ccb740b #x08761e9e #xbfa01e5f
              #xf17cfa14 #x2dbf99fb #x7a0be420 #x06137517 #xe020b266 #xd25bfc61 #xff10ed00 #x42e6be8b
              #x029ef587 #x683b26e0 #xb08afc70 #x7c1fd59e #xbaae9a70 #x98c8c801 #xb6e35a26 #x57083971
              #x90a6a680 #x1b44169e #x1dce237c #x518e0a59 #xccb11358 #x7b8175fb #xb8fe701a #x10d259bb
              #xe806ce10 #x9212be79 #x4604ae7b #x7fa22a84 #xe715b13a #x0394c3b2 #x11efbbae #xe13d9e19

              #x77e012bd #x2d05114c #xaecf2ddd #xb2a2b4aa #xb9429546 #x55dce815 #xc89138f8 #x46dcae20
              #x1f6f7162 #x0c557ebc #x5b996932 #xafbbe7e2 #xd2bd5f62 #xff475b9f #x9cec7108 #xeaddcffb
              #x5d751aef #xf68f7bdf #xf3f4e246 #x00983fcd #x00bc82bb #xbf5fd3e7 #xe80c7e2c #x187d8b1f
              #xefafb9a7 #x8f27a148 #x5c9606a9 #xf2d2be3e #xe992d13a #xe4bcd152 #xce40b436 #x63d6a1fc
              #xdc1455c4 #x64641e39 #xd83010c9 #x2d535ae0 #x5b748f3e #xf9a9146b #x80f10294 #x2859acd4
              #x5fc846da #x56d190e9 #x82167225 #x98e4daba #xbf7865f3 #x00da7ae4 #x9b7cd126 #x644172f8
              #xde40c78f #xe8803efc #xdd331a2b #x48485c3c #x4ed01ddc #x9c0b2d9e #xb1c6e9d7 #xd797d43c
              #x274101ff #x3bf7e127 #x91ebbc56 #x7ffeb321 #x4d42096f #xd6e9456a #x0bade318 #x2f40ee0b

              #x38cebf03 #x0cbc2e72 #xbf03e704 #x7b3e7a9a #x8e985acd #x90917617 #x413895f8 #xf11dde04
              #xc66f8244 #xe5648174 #x6c420271 #x2469d463 #x2540b033 #xdc788e7b #xe4140ded #x0990630a
              #xa54abed4 #x6e124829 #xd940155a #x1c8836f6 #x38fda06c #x5207ab69 #xf8be9342 #x774882a8
              #x56fc0d7e #x53a99d6e #x8241f634 #x9490954d #x447130aa #x8cc4a81f #x0868ec83 #xc22c642d
              #x47880140 #xfbff3bec #x0f531f41 #xf845a667 #x08c15fb7 #x1996cd81 #x86579103 #xe21dd863
              #x513d7f97 #x3984a1f1 #xdfcdc5f4 #x97766a5e #x37e2b1da #x41441f3f #xabd9ddba #x23b755a9
              #xda937945 #x103e650e #x3eef7c8f #x2760ff8d #x2493a4cd #x1d671225 #x3bf4bd4c #xed6e1728
              #xc70e9e30 #x4e05e529 #x928d5aa6 #x164d0220 #xb5184306 #x4bd7efb3 #x63830f11 #xf3a1526c

              #xf1545450 #xd41d5df5 #x25a5060d #x77b368da #x4fe33c7e #xeae09021 #xfdb053c4 #x2930f18d
              #xd37109ff #x8511a781 #xc7e7cdd7 #x6aeabc45 #xebbeaeaa #x9a0c4f11 #xda252cbb #x5b248f41
              #x5223b5eb #xe32ab782 #x8e6a1c97 #x11d3f454 #x3e05bd16 #x0059001d #xce13ac97 #xf83b2b4c
              #x71db5c9a #xdc8655a6 #x9e98597b #x3fcae0a2 #x75e63ccd #x076c72df #x4754c6ad #x26b5627b
              #xd818c697 #x998d5f3d #xe94fc7b2 #x1f49ad1a #xca7ff4ea #x9fe72c05 #xfbd0cbbf #xb0388ceb
              #xb76031e3 #xd0f53973 #xfb17907c #xa4c4c10f #x9f2d8af9 #xca0e56b0 #xb0d9b689 #xfcbf37a3
              #xfede8f7d #xf836511c #x744003fc #x89eba576 #xcfdcf6a6 #xc2007f52 #xaaaf683f #x62d2f9ca
              #xc996f77f #x77a7b5b3 #x8ba7d0a4 #xef6a0819 #xa0d903c0 #x01b27431 #x58fffd4c #x4827f45c

              #x44eb5634 #xae70edfc #x591c740b #x478bf338 #x2f3b513b #x67bf518e #x6fef4a0c #x1e0b6917
              #x5ac0edc5 #x2e328498 #x077de7d5 #x5726020b #x2aeda888 #x45b637ca #xcf60858d #x3dc91ae2
              #x3e6d5294 #xe6900d39 #x0f634c71 #x827a5fa4 #xc713994b #x1c363494 #x3d43b615 #xe5fe7d15
              #xf6ada4f2 #x472099d5 #x04360d39 #x7f2a71d0 #x88a4f5ff #x2c28fac5 #x4cd64801 #xfd78dd33
              #xc9bdd233 #x21e266cc #x9bbf419d #xcbf7d81d #x80f15f96 #x04242657 #x53fb0f66 #xded11e46
              #xf2fdba97 #x8d45c9f1 #x4eeae802 #x17003659 #xb9db81a7 #xe734b1b2 #x9503c54e #xb7c77c3e
              #x271dd0ab #xd8b906b5 #x0d540ec6 #xf03b86e0 #x0fdb7d18 #x95e261af #xad9ec04e #x381f4a64
              #xfec798d7 #x09ea20be #x0ef4ca57 #x1e6195bb #xfd0da78b #xcea1653b #x157d9777 #xf04af50f

              #xad7baa23 #xd181714a #x9bbdab78 #x6c7d1577 #x645eb1e7 #xa0648264 #x35839ca6 #x2287ef45
              #x32a64ca3 #x26111f6f #x64814946 #xb0cddaf1 #x4351c59e #x1b30471c #xb970788a #x30e9f597
              #xd7e58df1 #xc6d2b953 #xf5f37cf4 #x3d7c419e #xf91ecb2d #x9c87fd5d #xb22384ce #x8c7ac51c
              #x62c96801 #x57e54091 #x964536fe #x13d3b189 #x4afd1580 #xeba62239 #xb82ea667 #xae18d43a
              #xbef04402 #x1942534f #xc54bf260 #x3c8267f5 #xa1020ddd #x112fcc8a #xde596266 #xe91d0856
              #xf300c914 #xed84478e #x5b65009e #x4764da16 #xaf8e07a2 #x4088dc2c #x9a0cad41 #x2c3f179b
              #xa67b83f7 #xf27eab09 #xdbe10e28 #xf04c911f #xd1169f87 #x8e1e4976 #x17f57744 #xe4f5a33f
              #x27c2e04b #x0b7523bd #x07305776 #xc6be7503 #x918fa7c9 #xaf2e2cd9 #x82046f8e #xcc1c8250
        )
    )

(length known32)


(defun test-results (answers)
  (let* ((size (length answers))
        (buf (make-array size :element-type 'unsigned-byte :initial-element 0))
        (saw (make-array size :element-type 'unsigned-byte :initial-element 0)))
    (dotimes (i size)
      (setf (aref buf i) (logxor i #x80))
      (setf (aref saw i) (get-spooky-hash-32 buf i 0))
      (when (not (= (aref saw i) (aref *expected* i)))
        (format t "~3d: saw #x~8x, expected #x~8x~%" i (aref saw i) (aref *expected* i))))))

;; void TestResults()
;; {
;;     printf("\ntesting results ...\n");
;;     static const uint64 expected[BUFSIZE] = {
;;       0x6bf50919,0x70de1d26,0xa2b37298,0x35bc5fbf,0x8223b279,0x5bcb315e,0x53fe88a1,0xf9f1a233,
;;       0xee193982,0x54f86f29,0xc8772d36,0x9ed60886,0x5f23d1da,0x1ed9f474,0xf2ef0c89,0x83ec01f9,
;;       0xf274736c,0x7e9ac0df,0xc7aed250,0xb1015811,0xe23470f5,0x48ac20c4,0xe2ab3cd5,0x608f8363,
;;       0xd0639e68,0xc4e8e7ab,0x863c7c5b,0x4ea63579,0x99ae8622,0x170c658b,0x149ba493,0x027bca7c,
;;       0xe5cfc8b6,0xce01d9d7,0x11103330,0x5d1f5ed4,0xca720ecb,0xef408aec,0x733b90ec,0x855737a6,
;;       0x9856c65f,0x647411f7,0x50777c74,0xf0f1a8b7,0x9d7e55a5,0xc68dd371,0xfc1af2cc,0x75728d0a,
;;       0x390e5fdc,0xf389b84c,0xfb0ccf23,0xc95bad0e,0x5b1cb85a,0x6bdae14f,0x6deb4626,0x93047034,
;;       0x6f3266c6,0xf529c3bd,0x396322e7,0x3777d042,0x1cd6a5a2,0x197b402e,0xc28d0d2b,0x09c1afb4,
;;
;;       0x069c8bb7,0x6f9d4e1e,0xd2621b5c,0xea68108d,0x8660cb8f,0xd61e6de6,0x7fba15c7,0xaacfaa97,
;;       0xdb381902,0x4ea22649,0x5d414a1e,0xc3fc5984,0xa0fc9e10,0x347dc51c,0x37545fb6,0x8c84b26b,
;;       0xf57efa5d,0x56afaf16,0xb6e1eb94,0x9218536a,0xe3cc4967,0xd3275ef4,0xea63536e,0x6086e499,
;;       0xaccadce7,0xb0290d82,0x4ebfd0d6,0x46ccc185,0x2eeb10d3,0x474e3c8c,0x23c84aee,0x3abae1cb,
;;       0x1499b81a,0xa2993951,0xeed176ad,0xdfcfe84c,0xde4a961f,0x4af13fe6,0xe0069c42,0xc14de8f5,
;;       0x6e02ce8f,0x90d19f7f,0xbca4a484,0xd4efdd63,0x780fd504,0xe80310e3,0x03abbc12,0x90023849,
;;       0xd6f6fb84,0xd6b354c5,0x5b8575f0,0x758f14e4,0x450de862,0x90704afb,0x47209a33,0xf226b726,
;;       0xf858dab8,0x7c0d6de9,0xb05ce777,0xee5ff2d4,0x7acb6d5c,0x2d663f85,0x41c72a91,0x82356bf2,
;;
;;       0x94e948ec,0xd358d448,0xeca7814d,0x78cd7950,0xd6097277,0x97782a5d,0xf43fc6f4,0x105f0a38,
;;       0x9e170082,0x4bfe566b,0x4371d25f,0xef25a364,0x698eb672,0x74f850e4,0x4678ff99,0x4a290dc6,
;;       0x3918f07c,0x32c7d9cd,0x9f28e0af,0x0d3c5a86,0x7bfc8a45,0xddf0c7e1,0xdeacb86b,0x970b3c5c,
;;       0x5e29e199,0xea28346d,0x6b59e71b,0xf8a8a46a,0x862f6ce4,0x3ccb740b,0x08761e9e,0xbfa01e5f,
;;       0xf17cfa14,0x2dbf99fb,0x7a0be420,0x06137517,0xe020b266,0xd25bfc61,0xff10ed00,0x42e6be8b,
;;       0x029ef587,0x683b26e0,0xb08afc70,0x7c1fd59e,0xbaae9a70,0x98c8c801,0xb6e35a26,0x57083971,
;;       0x90a6a680,0x1b44169e,0x1dce237c,0x518e0a59,0xccb11358,0x7b8175fb,0xb8fe701a,0x10d259bb,
;;       0xe806ce10,0x9212be79,0x4604ae7b,0x7fa22a84,0xe715b13a,0x0394c3b2,0x11efbbae,0xe13d9e19,
;;
;;       0x77e012bd,0x2d05114c,0xaecf2ddd,0xb2a2b4aa,0xb9429546,0x55dce815,0xc89138f8,0x46dcae20,
;;       0x1f6f7162,0x0c557ebc,0x5b996932,0xafbbe7e2,0xd2bd5f62,0xff475b9f,0x9cec7108,0xeaddcffb,
;;       0x5d751aef,0xf68f7bdf,0xf3f4e246,0x00983fcd,0x00bc82bb,0xbf5fd3e7,0xe80c7e2c,0x187d8b1f,
;;       0xefafb9a7,0x8f27a148,0x5c9606a9,0xf2d2be3e,0xe992d13a,0xe4bcd152,0xce40b436,0x63d6a1fc,
;;       0xdc1455c4,0x64641e39,0xd83010c9,0x2d535ae0,0x5b748f3e,0xf9a9146b,0x80f10294,0x2859acd4,
;;       0x5fc846da,0x56d190e9,0x82167225,0x98e4daba,0xbf7865f3,0x00da7ae4,0x9b7cd126,0x644172f8,
;;       0xde40c78f,0xe8803efc,0xdd331a2b,0x48485c3c,0x4ed01ddc,0x9c0b2d9e,0xb1c6e9d7,0xd797d43c,
;;       0x274101ff,0x3bf7e127,0x91ebbc56,0x7ffeb321,0x4d42096f,0xd6e9456a,0x0bade318,0x2f40ee0b,
;;
;;       0x38cebf03,0x0cbc2e72,0xbf03e704,0x7b3e7a9a,0x8e985acd,0x90917617,0x413895f8,0xf11dde04,
;;       0xc66f8244,0xe5648174,0x6c420271,0x2469d463,0x2540b033,0xdc788e7b,0xe4140ded,0x0990630a,
;;       0xa54abed4,0x6e124829,0xd940155a,0x1c8836f6,0x38fda06c,0x5207ab69,0xf8be9342,0x774882a8,
;;       0x56fc0d7e,0x53a99d6e,0x8241f634,0x9490954d,0x447130aa,0x8cc4a81f,0x0868ec83,0xc22c642d,
;;       0x47880140,0xfbff3bec,0x0f531f41,0xf845a667,0x08c15fb7,0x1996cd81,0x86579103,0xe21dd863,
;;       0x513d7f97,0x3984a1f1,0xdfcdc5f4,0x97766a5e,0x37e2b1da,0x41441f3f,0xabd9ddba,0x23b755a9,
;;       0xda937945,0x103e650e,0x3eef7c8f,0x2760ff8d,0x2493a4cd,0x1d671225,0x3bf4bd4c,0xed6e1728,
;;       0xc70e9e30,0x4e05e529,0x928d5aa6,0x164d0220,0xb5184306,0x4bd7efb3,0x63830f11,0xf3a1526c,
;;
;;       0xf1545450,0xd41d5df5,0x25a5060d,0x77b368da,0x4fe33c7e,0xeae09021,0xfdb053c4,0x2930f18d,
;;       0xd37109ff,0x8511a781,0xc7e7cdd7,0x6aeabc45,0xebbeaeaa,0x9a0c4f11,0xda252cbb,0x5b248f41,
;;       0x5223b5eb,0xe32ab782,0x8e6a1c97,0x11d3f454,0x3e05bd16,0x0059001d,0xce13ac97,0xf83b2b4c,
;;       0x71db5c9a,0xdc8655a6,0x9e98597b,0x3fcae0a2,0x75e63ccd,0x076c72df,0x4754c6ad,0x26b5627b,
;;       0xd818c697,0x998d5f3d,0xe94fc7b2,0x1f49ad1a,0xca7ff4ea,0x9fe72c05,0xfbd0cbbf,0xb0388ceb,
;;       0xb76031e3,0xd0f53973,0xfb17907c,0xa4c4c10f,0x9f2d8af9,0xca0e56b0,0xb0d9b689,0xfcbf37a3,
;;       0xfede8f7d,0xf836511c,0x744003fc,0x89eba576,0xcfdcf6a6,0xc2007f52,0xaaaf683f,0x62d2f9ca,
;;       0xc996f77f,0x77a7b5b3,0x8ba7d0a4,0xef6a0819,0xa0d903c0,0x01b27431,0x58fffd4c,0x4827f45c,
;;
;;       0x44eb5634,0xae70edfc,0x591c740b,0x478bf338,0x2f3b513b,0x67bf518e,0x6fef4a0c,0x1e0b6917,
;;       0x5ac0edc5,0x2e328498,0x077de7d5,0x5726020b,0x2aeda888,0x45b637ca,0xcf60858d,0x3dc91ae2,
;;       0x3e6d5294,0xe6900d39,0x0f634c71,0x827a5fa4,0xc713994b,0x1c363494,0x3d43b615,0xe5fe7d15,
;;       0xf6ada4f2,0x472099d5,0x04360d39,0x7f2a71d0,0x88a4f5ff,0x2c28fac5,0x4cd64801,0xfd78dd33,
;;       0xc9bdd233,0x21e266cc,0x9bbf419d,0xcbf7d81d,0x80f15f96,0x04242657,0x53fb0f66,0xded11e46,
;;       0xf2fdba97,0x8d45c9f1,0x4eeae802,0x17003659,0xb9db81a7,0xe734b1b2,0x9503c54e,0xb7c77c3e,
;;       0x271dd0ab,0xd8b906b5,0x0d540ec6,0xf03b86e0,0x0fdb7d18,0x95e261af,0xad9ec04e,0x381f4a64,
;;       0xfec798d7,0x09ea20be,0x0ef4ca57,0x1e6195bb,0xfd0da78b,0xcea1653b,0x157d9777,0xf04af50f,
;;
;;       0xad7baa23,0xd181714a,0x9bbdab78,0x6c7d1577,0x645eb1e7,0xa0648264,0x35839ca6,0x2287ef45,
;;       0x32a64ca3,0x26111f6f,0x64814946,0xb0cddaf1,0x4351c59e,0x1b30471c,0xb970788a,0x30e9f597,
;;       0xd7e58df1,0xc6d2b953,0xf5f37cf4,0x3d7c419e,0xf91ecb2d,0x9c87fd5d,0xb22384ce,0x8c7ac51c,
;;       0x62c96801,0x57e54091,0x964536fe,0x13d3b189,0x4afd1580,0xeba62239,0xb82ea667,0xae18d43a,
;;       0xbef04402,0x1942534f,0xc54bf260,0x3c8267f5,0xa1020ddd,0x112fcc8a,0xde596266,0xe91d0856,
;;       0xf300c914,0xed84478e,0x5b65009e,0x4764da16,0xaf8e07a2,0x4088dc2c,0x9a0cad41,0x2c3f179b,
;;       0xa67b83f7,0xf27eab09,0xdbe10e28,0xf04c911f,0xd1169f87,0x8e1e4976,0x17f57744,0xe4f5a33f,
;;       0x27c2e04b,0x0b7523bd,0x07305776,0xc6be7503,0x918fa7c9,0xaf2e2cd9,0x82046f8e,0xcc1c8250
;;     };
;;
;;     uint8 buf[BUFSIZE];
;;     uint32 saw[BUFSIZE];
;;     for (int i=0; i<BUFSIZE; ++i)
;;     {
;;         buf[i] = i+128;
;;         saw[i] = SpookyHash::Hash32(buf, i, 0);
;;         if (saw[i] != expected[i])
;;         {
;;   	    printf("%3d: saw 0x%.8lx, expected 0x%.8lx\n", i, saw[i], expected[i]);
;;         }
;;     }
;; }

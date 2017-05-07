(ns alphabet-cipher.coder)

(defn char-to-int
  "Converts a char to a 0-indexed int. E.g. \\a == 0, \\b == 1 and so
  on"
  [char]
  (- (int char) 97))

(defn int-to-char
  "Converts an int modulus 26 into the corresponding char"
  [int]
  (char (+ (mod int 26) 97)))

(defn encode-char
  "encodes a single char using the alphabet cipher and a provided key"
  [char key]
  (let [char-int (char-to-int char)
        key-int (char-to-int key)]
    (int-to-char (+ char-int key-int))))

(defn decode-char
  "decodes a single char using the alphabet cipher and a provided key"
  [char key]
  (let [char-int (char-to-int char)
        key-int (char-to-int key)]
    (int-to-char (- char-int key-int))))

(defn alpha-cipher
  [cipher-fn]
  (fn [keyword message]
    (let [message-len (count message)
          keyword (take message-len (cycle keyword))
          message (seq message)]
      (->> (map cipher-fn message keyword)
           (apply str)))))

(defn encode
  [keyword message]
  (let [cipher (alpha-cipher encode-char)]
    (cipher keyword message)))

(defn decode
  [keyword message]
  (let [cipher (alpha-cipher decode-char)]
    (cipher keyword message)))

(defn decipher
  [encoded-message message]
  (let [cipher (alpha-cipher decode-char)
        keyword-cycle (seq (cipher message encoded-message))
        cycle-reducer (fn [keyword next-char]
                        (let [cycle-length (count keyword-cycle)]
                          (if (= (take cycle-length (cycle keyword))
                                 keyword-cycle)
                            (reduced keyword)
                            (conj keyword next-char))))]
    (apply str (reduce cycle-reducer [(first keyword-cycle)] (rest keyword-cycle)))))

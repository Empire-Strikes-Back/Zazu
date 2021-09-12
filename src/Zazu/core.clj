(ns Zazu.core
  (:require 
    [clojure.core.async :as a :refer [<! >! <!! >!! chan put! take! go alt! alts! do-alts close! timeout pipe mult tap untap 
                                      pub sub unsub mix admix unmix dropping-buffer sliding-buffer pipeline pipeline-async to-chan! thread]]
    [clojure.string]
    [clojure.java.io :as io]
    )
  (:import 
    (java.awt.image BufferedImage)
    (java.awt Image Graphics2D Color)
    (javax.imageio ImageIO)
    (java.security MessageDigest)
  )  
)

(do (set! *warn-on-reflection* true) (set! *unchecked-math* true))

(defn process
  [opts]
  (let [default-opts {:word nil
                      :filename nil
                      :size nil
                      }
        {:keys [word filename size]} (merge default-opts opts)]

    (let [digest (-> 
                  (doto (MessageDigest/getInstance "SHA-256")
                    (.update (.getBytes ^String word))
                  )
                  (.digest)
                  )
          color (let [size (alength digest)
                      step 3
                      n (- size (mod size 3))
                      positions (range 0 n step)
                      positions-size (count positions)
                      colors (->>
                                (reduce
                                (fn [result i]
                                  (-> result
                                    (update :red + (bit-and (aget digest i) 0xff))
                                    (update :green + (bit-and (aget digest (+ i 1)) 0xff))
                                    (update :blue + (bit-and (aget digest (+ i 2)) 0xff))
                                  )
                                )
                                {:red 0 :green 0 :blue 0}
                                positions
                                )
                              (map (fn [[k value]]
                                [k (-> value (/ positions-size)) ]
                                ))
                              (into {})
                              )
                      ]
                  (Color. (int (:red colors)) (int (:green colors)) (int (:blue colors)))
                  )
          half-width 4
          width (* half-width 2)
          height 8
          padding 1
          width-with-padding (+ width padding padding)
          height-with-padding (+ height padding padding)
          image (BufferedImage. width-with-padding height-with-padding BufferedImage/TYPE_INT_ARGB)
          graphics (.createGraphics image)
          _ (.setPaint graphics (Color. 255 255 255))
          _ (.fillRect graphics 0 0 (.getWidth image) (.getHeight image))
          _ (doseq [x (range 0 half-width)
                    y (range 0 height) 
                    :let [position (+ x (* y half-width)) ]]
                (when (pos? (aget digest position) )
                  (.setRGB image (+ x padding) (+ y padding) (.getRGB color) )
                  (.setRGB image (- width padding x) (+ y padding) (.getRGB color) )
                )
              )
          scaled-image (.getScaledInstance image size size Image/SCALE_FAST)
          write-image (BufferedImage. (.getWidth scaled-image nil) (.getHeight scaled-image nil)  BufferedImage/TYPE_INT_ARGB)
          graphics (.createGraphics write-image)
          _ (.drawImage graphics scaled-image 0 0 nil)
          _ (.dispose graphics)
          ]
      (io/make-parents filename)
      (ImageIO/write write-image "png" (io/file filename))
     
    )
  )
)
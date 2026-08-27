(ns om.test-env
  "A DOM and the `js/React` globals om reads, for the node test build.

   Loaded before any `defui` form evaluates, because `defui` reads
   `js/React.Component` when it defines the class. `REACT_VERSION` is
   informational only — whichever react/react-dom `node_modules` holds is the one
   installed; see package.json's `test:react-matrix`."
  (:require [goog.object :as gobj]))

(defn- define! [name value]
  (js/Object.defineProperty js/globalThis name
    #js {:value value :writable true :configurable true}))

(defn- install-dom! []
  (let [JSDOM (.-JSDOM (js/require "jsdom"))
        dom   (JSDOM. "<!doctype html><html><body></body></html>"
                #js {:pretendToBeVisual true})
        w     (.-window dom)]
    (define! "window" w)
    (define! "document" (.-document w))
    (define! "navigator" (.-navigator w))
    (define! "HTMLElement" (.-HTMLElement w))
    (define! "Element" (.-Element w))
    (define! "Node" (.-Node w))
    (define! "Event" (.-Event w))
    (define! "MutationObserver" (.-MutationObserver w))
    (define! "requestAnimationFrame" (fn [f] (.requestAnimationFrame w f)))
    (define! "cancelAnimationFrame" (fn [h] (.cancelAnimationFrame w h)))
    ;; React 18+ reads this to decide whether to warn about updates outside act().
    (define! "IS_REACT_ACT_ENVIRONMENT" false)
    (define! "__om_test_teardown" #(.close w))))

(defn- install-react! []
  (let [react     (js/require "react")
        react-dom (js/require "react-dom")
        version   (.-version react)
        major     (js/parseInt (first (.split version ".")) 10)]
    ;; node keeps searching outward when a package lacks the requested subpath,
    ;; so a react-dom from an enclosing node_modules can answer for one that has
    ;; no `client` — pairing react 16 with a react-dom that renders nothing it
    ;; produced. Resolve by version rather than by whether a require succeeds.
    (when-not (= version (gobj/get react-dom "version"))
      (throw (ex-info "react and react-dom versions disagree; check for a react-dom in an enclosing node_modules"
               {:react version :react-dom (gobj/get react-dom "version")})))
    (define! "React" react)
    (define! "ReactDOM" react-dom)
    (define! "ReactDOMClient"
      (when (>= major 18)
        (js/require "react-dom/client")))))

(defonce ^:private installed
  (do (install-dom!)
      (install-react!)
      ;; A run says which runtime it tested; otherwise a green suite names no
      ;; version and a version-dependent result cannot be read back.
      (println "om test-env: react" (.-version js/React)
               "| react-dom" (gobj/get js/ReactDOM "version")
               "| react-dom/client" (if (some? js/ReactDOMClient) "yes" "no"))
      true))

(defn react-major []
  (js/parseInt (first (.split (.-version js/React) ".")) 10))

(defn concurrent-root-available? []
  (some? js/ReactDOMClient))

(defn fresh-target!
  "A new mount point. React keeps per-container state, so each test wants its own."
  []
  (let [el (.createElement js/document "div")]
    (.appendChild (.-body js/document) el)
    el))

(defn wait-for
  "Polls `pred` until it is truthy, then calls `k` with true; calls it with false
   once `timeout-ms` has passed. Keeps timing out of the assertions: a test states
   what it waits for rather than how long om takes to get there."
  ([pred k] (wait-for pred k 2000))
  ([pred k timeout-ms]
   (let [deadline (+ (js/Date.now) timeout-ms)
         step     (fn step []
                    (cond
                      (pred)                     (k true)
                      (> (js/Date.now) deadline) (k false)
                      :else                      (js/setTimeout step 5)))]
     (step))))

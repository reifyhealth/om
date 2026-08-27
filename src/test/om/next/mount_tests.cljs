(ns om.next.mount-tests
  "Reconciler tests that mount a real react-dom tree into a jsdom document.

   Nothing here stubs `om.next/mounted?` or installs reconciler state by hand: a
   test adds a root, transacts, and waits for the DOM. That is what lets these
   tests see behaviour `om.next.tests` cannot — the liveness check, the
   scheduler and the render path are all live."
  (:require [cljs.test :refer-macros [deftest is testing async]]
            [om.test-env :as env]
            [om.next :as om :refer-macros [defui]]
            [om.next.protocols :as p]))

;; -----------------------------------------------------------------------------
;; Fixture

(def initial-state
  {:items      [[:item/by-id 1] [:item/by-id 2]]
   :item/by-id {1 {:id 1 :label "one"}
                2 {:id 2 :label "two"}}})

(defn reader
  [{:keys [state query target]} k _]
  (when (nil? target)
    (let [st @state]
      (case k
        :items           {:value (om/db->tree query (get st :items) st)}
        ;; A key no component queries, so the indexer resolves it to nothing.
        :nothing/queried {:value :sentinel}
        {:value (get st k)}))))

(defn mutate
  [{:keys [state ref]} k params]
  (case k
    item/rename       {:action #(swap! state assoc-in (conj (vec ref) :label) (:label params))}
    root/rename-first {:action #(swap! state assoc-in [:item/by-id 1 :label] (:label params))}
    nil))

(defonce children (atom []))
(defonce liveness (atom {}))

(defui Child
  static om/Ident
  (ident [_ props] [:item/by-id (:id props)])
  static om/IQuery
  (query [_] [:id :label])
  Object
  (componentWillMount [this]
    (swap! liveness assoc :will-mount (om/mounted? this)))
  (componentDidMount [this]
    (swap! children conj this)
    (swap! liveness assoc :did-mount (om/mounted? this)))
  (componentWillUnmount [this]
    (swap! liveness assoc :will-unmount (om/mounted? this)))
  (render [this]
    (swap! liveness assoc :render (om/mounted? this))
    (js/React.createElement "span" nil (str (:label (om/props this)) " "))))

(def child-factory (om/factory Child))

(defui Root
  static om/IQuery
  (query [_] [{:items (om/get-query Child)}])
  Object
  (render [this]
    (apply js/React.createElement "div" nil
      (map child-factory (:items (om/props this))))))

;; -----------------------------------------------------------------------------
;; Host

(defn- app
  "Mounts Root and returns the pieces a test drives it with. `:root-render` wraps
   what om would call anyway, only to count root renders — from the DOM alone the
   targeted path and the root path are indistinguishable.

   `:concurrent? true` gives a react-dom/client root, whose render is
   asynchronous and returns no component instance."
  [& {:keys [concurrent?]}]
  (reset! children [])
  (reset! liveness {})
  (let [target  (env/fresh-target!)
        renders (atom 0)
        roots   (atom {})
        render  (if concurrent?
                  (fn [c t]
                    (let [r (or (get @roots t)
                                (let [r (.createRoot js/ReactDOMClient t)]
                                  (swap! roots assoc t r)
                                  r))]
                      (.render r c)
                      nil))
                  (fn [c t] (js/ReactDOM.render c t)))
        r       (om/reconciler
                  {:state        (atom initial-state)
                   :parser       (om/parser {:read reader :mutate mutate})
                   :root-render  (fn [c t] (swap! renders inc) (render c t))
                   :root-unmount (fn [t]
                                   (if concurrent?
                                     (some-> (get @roots t) (.unmount))
                                     (js/ReactDOM.unmountComponentAtNode t)))})]
    (om/add-root! r Root target)
    {:reconciler r :target target :renders renders}))

(defn- text [target]
  (.-textContent target))

(defn- shows? [target s]
  (fn [] (some? (re-find (re-pattern s) (text target)))))

;; -----------------------------------------------------------------------------
;; Liveness

(deftest test-mounted-through-the-lifecycle
  (testing "mounted? answers for a live component, from componentWillMount on"
    (let [{:keys [reconciler target]} (app)
          c (first @children)]
      (is (= true (:will-mount @liveness))
        "a componentWillMount body sees itself as mounted; om indexes there, so a
         transaction can target the component before its first commit")
      (is (= true (:render @liveness)))
      (is (= true (:did-mount @liveness)))
      (is (true? (om/mounted? c)))
      (testing "and stops answering once the component is gone"
        (om/remove-root! reconciler target)
        (is (= true (:will-unmount @liveness))
          "componentWillUnmount runs while still mounted, as React defines it")
        (is (false? (om/mounted? c)))))))

;; -----------------------------------------------------------------------------
;; The targeted path

(deftest test-component-transact-reaches-the-dom
  (testing "a component-initiated transact! updates that component, not the root"
    (async done
      (let [{:keys [target renders]} (app)
            c      (first @children)
            before @renders]
        (om/transact! c '[(item/rename {:label "renamed"})])
        (env/wait-for (shows? target "renamed")
          (fn [ok]
            (is ok (str "expected the child to re-render; DOM is " (pr-str (text target))))
            (is (= before @renders)
              "the queue resolves to the component, so this is the targeted path")
            (done)))))))

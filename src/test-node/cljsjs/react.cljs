(ns cljsjs.react
  "Stands in for the cljsjs packaging on the node test build's source path, so
   `om.next.tests` resolves its require. `om.test-env` supplies `js/React` from
   node_modules."
  (:require [om.test-env]))

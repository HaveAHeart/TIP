### На поделать:

---

- Допишите реализацию [live variables analysis](../src/tip/analysis/LiveVarsAnalysis.scala) в `LiveVarsAnalysis.scala`.

Созданная реализация протестирована на [liveness.tip](../examples/liveness.tip), результат соответствует ожидаемому. 

---

- Реализуйте [reaching definitions analysis](../src/tip/analysis/ReachingDefinitionsAnalysis.scala) в `ReachingDefinitionsAnalysis.scala`.

Помимо самой реализации класса пришлось раскомментировать и слегка исправить строки для возможности запуска соответствующего анализа в файле [FlowSensitiveAnalysis.scala](../src/tip/analysis/FlowSensitiveAnalysis.scala).

Также стоит отметить, что `Reaching definitions`, в отличии от `Live variables` - forward-анализ, что выражается в использовании других `Dependencies` для solver'ов.

Реализация протестирована на [reaching.tip](../examples/reaching.tip) и [simpleReaching.tip](../examples/simpleReaching.tip), результат соответствует ожидаемому. 
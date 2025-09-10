# async1

**async1** is an Emacs Lisp package for building static pipelines of asynchronous functions, supporting both sequential and parallel execution patterns. It provides a simple interface to compose async chains of callbacks, with customizable result aggregation for parallel steps.

Async functions don't return and callbacks don't return "futures/promises". Instead,  together they build chain of calls that executed according to pipeline, asynchronous.

Other Emacs packages with solution for "callback hell":
- deferred https://github.com/kiwanami/emacs-deferred
- promise https://github.com/chuntaro/emacs-promise
- aio https://github.com/skeeto/emacs-aio

## Why?

Here's a small example of "callback hell" in Emacs Lisp with nested `url-retrieve` calls:
```elisp
(url-retrieve "http://example.com/1"
  (lambda (status1)
    ;; do something with first response...
    (url-retrieve "http://example.com/2"
      (lambda (status2)
        ;; do something with second response...
        (url-retrieve "http://example.com/3"
          (lambda (status3)
            ;; do something with third response...
            (message "Done!")))))))
```

Remote LLMs or AI Agents may require chains of request for prompt-engineering.

## What?
**1. Sequential and parallel steps**
```elisp
(async1-start nil
    '((:result "Step 1" :delay 1)
         (:parallel
             (:result "Parallel A" :delay 2)
             (
                 (:result "Sub-seq a" :delay 1)
                 (:result "Sub-seq b" :delay 1)
                 ;; custom-async-step ;; or your function like this
             )
             (:result "Parallel B" :delay 2))
         (:result "Step 3" :delay 1)))

;; Output: "Final result: {Step 1 -> Sub-seq a -> Sub-seq b, Step 1 -> Parallel B, Step 1 -> Parallel A} -> Step 3"
```

Here ```(:result "Step 1" :delay 1)``` entities is just examples of ```(lambda (text callback))``` that output text and run callback with delay.

Define and run an async pipeline with `async1-start`. Each step must be a record or function with `(data callback)` signature.

## Features
- **Deep chain trees**
- **Parallel and sequential steps**
- **Custom async steps**: Use your own functions that should take `(data callback)`, where callback is the next step function that should be called.
- **Custom aggregator**: Control how parallel results are combined. Redefine `async-default-aggregator` or pass your own function with `:aggregator` in parallel lists.
- **Custom final callback**: function that receive only data to do something with result.
- **External data handling**: Lambdas can capture variables.

## Configuration
```elisp
(require 'async1)
```

## Installation variants

### Copy file by hands:
```elisp
(add-to-list 'load-path "/path/to/async1/async1.el")
(require 'async1)
```

### From MELPA full steps

1.  Make sure MELPA is in your package archives:
    ```elisp
    (require 'package)
    (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
    (package-initialize)
    ```

2.  Install via `M-x package-install RET async1 RET`

### With `use-package`
If your package is available on MELPA, add this to your init file:

```elisp
(use-package async1
  :ensure t)
```

If installing from a GitHub repo (not yet in MELPA), specify the source:
```elisp
(use-package async1
  :straight (async1 :host github :repo "Anoncheg1/emacs-async1"))
;; Requires straight.el. See below.
```

### With `straight.el`
Add to your init file, replacing `yourusername` with your GitHub username:

```elisp
(straight-use-package
 '(async1 :host github :repo "Anoncheg1/emacs-async1"))
```

Or with [Quelpa](https://github.com/quelpa/quelpa):
```elisp
(quelpa '(async1 :repo "Anoncheg1/emacs-async1" :fetcher github))
```

## Usage examples

**2. Mixing custom async functions**
```elisp
(defun custom-async-step (data callback)
  (run-at-time 1.5 nil callback
               (concat data " -> Custom Step")))

(async1-start nil
  '((:result "Step 1" :delay 1)
    (:parallel
     custom-async-step
     (:result "Parallel B" :delay 1))
    (:result "Step 3" :delay 1)))
```

**3. Custom aggregators**
```elisp
(defun custom-aggregator (results)
  (mapconcat 'identity results " & "))

(async1-start nil
  '((:result "Step 1" :delay 1)
    (:parallel
     (:result "Parallel A" :delay 1)
     (:result "Parallel B" :delay 2)
     :aggregator #'custom-aggregator)))
;; Output: "Final result: Step 1 -> Parallel B & Step 1 -> Parallel A"
```

**4. Using external data defined with lambdas**
```elisp
(let* ((var "myvar")
       (stepcallback)
       (callback1 (lambda (data)
                    (funcall stepcallback (concat data " -> " var))))
       (call (lambda (data callback)
               (setq stepcallback callback)
               (run-at-time 0 nil callback1
                            (concat data " -> " "Step1")))))
  (async1-start nil
                (list call call call)))
;; Output: "Final result:  -> Step1 -> myvar -> Step1 -> myvar -> Step1 -> myvar"
```

**5. Mutable lambdas**
```elisp
(let* ((call (lambda (step)
               (lambda (data callback)
                 (run-at-time 0 nil callback
                              (concat data " -> " "Step" (number-to-string step)))))))
  (async1-start nil
                (list (funcall call 0)
                      (funcall call 1)
                      (funcall call 2)
                      (funcall call 3))))
;; Output: "Final result:  -> Step0 -> Step1 -> Step2 -> Step3"
```

Battlefield example: https://github.com/Anoncheg1/oai/blob/main/oai-prompt.el

## TODO / Roadmap
- Think how `:catch` (error handling) may be useful and implemented.

## License
GNU Affero General Public License, version 3 (AGPLv3)

## Donate, sponsor author
You can give me crypto money directly with crypto currencies:
- BTC (Bitcoin) address: 1CcDWSQ2vgqv5LxZuWaHGW52B9fkT5io25
- USDT (Tether) address: TVoXfYMkVYLnQZV3mGZ6GvmumuBfGsZzsN

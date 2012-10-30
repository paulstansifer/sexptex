#lang racket

(provide (all-defined-out))


;; join
(define j
  (match-lambda**
   [(sep `(,a ,b . ,rest))
    `(,a ,sep ,@(j sep `(,b . ,rest)))]
   [(sep `(,last))
    `(,last)]
   [(sep `())
    `()]))

;;yes, it's used that much; it works as concatenation
(define ~ list)



(define (str-flatten . sexp)
  (string-append*
   (map
    translate-char
    (string->list
     (string-append* (flatten sexp))))))

;; TeX macro invocation
;; bs : Symbol × [String] → String
(define (bs macname . args)
  `("\\" ,(symbol->string macname)
    ,(map (λ (arg) `("{" ,arg "}"))
          args)
    ,(if (empty? args) " " "")))

;; bso : Symbol × [String] × [String] → String
(define (bso macname opts . args)
  `("\\" ,(symbol->string macname)
    "[" ,(j "," opts) "]"
    ,(map (λ (arg) `("{" ,arg "}"))
          args)))

;; __ : Math → Math ∩ Text
(define (__ . body)
  ($ "_{" body "}"))

;; ^^ : Math → Math ∩ Text
(define (^^ . body)
  ($ "^{" body "}"))

;; ⋃ : Math × Math → Math ∩ Text
(define (⋃ range body)
  (~ "\\bigcup\\limits_{" range "}" body))

;; ⋃s : Math → Math ∩ Text
(define (⋃s body)
  (~ "\\bigcup" body))

(define ($ . body)
  (bs 'ensuremath body))

(define (text . txt)
  (bs 'textup txt))

(define ($$ . body)
  (~ "\\[" body "\\]"))

(define (env name . body)
  `(,(bs 'begin (symbol->string name)) "\n"
    ,body "\n"
    ,(bs 'end (symbol->string name)) "\n"))

(define end "\\\\\n")
(define ¶ "\n\n")

(define (ital . texts)
  (~ (bs 'textit texts )))

(define (bold . texts)
  (~ (bs 'textbf texts )))

(define-syntax-rule (items (item-parts ...) ...)
  (env 'itemize
       (~ (bs 'item) item-parts ...) ...
       ))

;; requires mathpartir
(define (mathpar . parts)
  (env 'mathpar (j "\\and" parts)))

(define (array spec . chunks)
  ($
   (env 'array
        "{" (string-append* (map symbol->string spec)) "}"
        (let r ((chunks chunks))
          (if (empty? chunks) '()
              (let-values (((here rest) (split-at chunks (length spec))))
                `(,(j "&" here) ,end
                  ,(r rest))))))))

(define (parray spec . chunks)
  (~ "\\[" (apply array `(,spec . ,chunks)) "\\]"))

;; named inference rule — requires mathpartir
(define (top
         . ---- . name
         bot)
  (bso 'inferrule* `(("right=" ,(bs 'textsc name)))
       top
       bot))

;; function invocation
(define (fn name . args)
  `(,(bs 'operatorname name) "\\left(" ,(j "," args) "\\right)"))

(define (fnname name)
  (bs 'operatorname name))

;;delimiters
(define (|(| . elts)
  (~ "\\left(" elts "\\right)"))
(define (|[| . elts)
  (~ "\\left[" elts "\\right]"))
(define (|{| . elts)
  (~ "\\left\\{" elts "\\right\\}"))
(define (angle . elts)
  (~ "\\left\\langle " elts "\\right\\rangle"))

(define |()| |(|)
(define |[]| |[|)
(define |{}| |{|)
(define 〈〉 angle)
(define (〚〛 . elts) ;;requires stmaryrd
  (~ "\\left〚" elts "\\right〛"))
(define interp 〚〛)


(define (abs . elts)
  (~ "\\left|" elts "\\right|"))

(define (∀ binder #:st [such-that #f] body)
  (~ "∀" binder (if such-that (~ ", " such-that) "") "." body))
(define (∃ binder #:st [such-that #f] body)
  (~ "∃" binder (if such-that (~ ", " such-that) "") "." body))



;; mathematical conditional
(define-syntax-rule
  (cases (cnd val) ...)
  (env 'cases
       (j end
          `((,val " & " ,cnd) ...))))


(define-syntax-rule (doc class args pkgs defs body ...)
  (display
   (str-flatten
    (bso 'documentclass args class)
    (map (λ (pkg) (bs 'usepackage (symbol->string pkg))) pkgs)
    defs
    (env 'document body ...))))



(define (mathify str)
  (string-append "\\ensuremath{" str "}"))

;;todo: handle \big*
(define (unsafe-mathify str)
  (string-append "\\ensuremath{" str "}"))

(define (translate-char c)
  (case c
    ;; Don't escape latex commands
    #;[(#\\) (mathify "\\backslash")]
    #;[(#\_) (mathify "\\_")]
    #;[(#\^) "{\\char'136}"]
    #;[(#\>) (mathify ">")]
    #;[(#\<) (if (rendering-tt) "{\\texttt <}" "$<$")]
    #;[(#\|) (if (rendering-tt) "{\\texttt |}" "$|$")]
    #;[(#\-) "{-}"] ;; avoid en- or em-dash
    #;[(#\`) "{`}"] ;; avoid double-quotes
    #;[(#\') "{'}"] ;; avoid double-quotes
    #;[(#\? #\! #\. #\:)
    (if (rendering-tt) (format "{\\hbox{\\texttt{~a}}}" c) c)]
    #;[(#\~) "$\\sim$"]
    #;[(#\{ #\}) (if (rendering-tt)
    (format "{\\char`\\~a}" c)
    (format "\\~a" c))]
            #;[(#\[ #\]) (if (escape-brackets)
                           (if (eq? c #\[)
                               "{\\SOpenSq}"
                               "{\\SCloseSq}")
                           c)]
            #;[(#\# #\% #\& #\$) (format "\\~a" c)]
    [(#\uA0) "~"] ; non-breaking space
    [(#\uAD) "\\-"] ; soft hyphen; unfortunately, also disables auto-hyphen
    [(#\uDF) "{\\ss}"]
    [else
     (if ((char->integer c) . <= . 127)
         (string c)
         (case c
           [(#\〚) (mathify "[\\!\\![")]
           [(#\〛) (mathify "]\\!\\!]")]
           [(#\u2011) "\\mbox{-}"] ; non-breaking hyphen
           [(#\uB0) (mathify "^{\\circ}")] ; degree
           [(#\uB2) (mathify "^2")]
           [(#\u039A) "K"] ; kappa
           [(#\u0391) "A"] ; alpha
           [(#\u039F) "O"] ; omicron
           [(#\u03A3) (mathify "\\Sigma")]
           [(#\u03BA) (mathify "\\kappa")]
           [(#\u03B1) (mathify "\\alpha")]
           [(#\u03B2) (mathify "\\beta")]
           [(#\u03B3) (mathify "\\gamma")]
           [(#\u03BF) "o"] ; omicron
           [(#\u03C3) (mathify "\\sigma")]
           [(#\u03C2) (mathify "\\varsigma")]
           [(#\u03BB) (mathify "\\lambda")]
           [(#\u039B) (mathify "\\Lambda")]
           [(#\u03BC) (mathify "\\mu")]
           [(#\u03C0) (mathify "\\pi")]
           [(#\‘) "{`}"]
           [(#\’) "{'}"]
           [(#\“) "{``}"]
           [(#\”) "{''}"]
           [(#\u2013) "{--}"]
           [(#\u2014) "{---}"]
           [(#\∣) (mathify "\\mid")]
           [(#\·) (mathify "\\cdot")]
           [(#\〈) (mathify "\\langle")]
           [(#\〉) (mathify "\\rangle")]
           [(#\⋯) (mathify "\\cdots")]
           [(#\∞) (mathify "\\infty")]
           ;; Arrows
           [(#\⇓) (mathify "\\Downarrow")]
           [(#\↖) (mathify "\\nwarrow")]
           [(#\↓) (mathify "\\downarrow")]
           [(#\⇒) (mathify "\\Rightarrow")]
           [(#\→) (mathify "\\rightarrow")]
           [(#\↝) (mathify "\\rightsquigarrow")]
           [(#\↘) (mathify "\\searrow")]
           [(#\↙) (mathify "\\swarrow")]
           [(#\←) (mathify "\\leftarrow")]
           [(#\↑) (mathify "\\uparrow")]
           [(#\⇐) (mathify "\\Leftarrow")]
           [(#\−) (mathify "\\longrightarrow")]
           [(#\⇑) (mathify "\\Uparrow")]
           [(#\⇔) (mathify "\\Leftrightarrow")]
           [(#\↕) (mathify "\\updownarrow")]
           [(#\↔) (mathify "\\leftrightarrow")]
           [(#\↗) (mathify "\\nearrow")]
           [(#\⇕) (mathify "\\Updownarrow")]
           [(#\⇚) (mathify "\\Lleftarrow")]
           [(#\⇛) (mathify "\\Rrightarrow")]
           [(#\↦) (mathify "\\mapsto")]
           [(#\⤇) (mathify "\\Mapsto")]
           [(#\⟼) (mathify "\\longmapsto")]
           [(#\⟾) (mathify "\\Longmapsto")]
           [(#\⇆) (mathify "\\leftrightarrows")]
           ;; Harpoons
           [(#\⇀) (mathify "\\rightharpoonup")]
           [(#\↼) (mathify "\\leftharpoonup")]
           [(#\↾) (mathify "\\uprightharpoon")]
           [(#\↿) (mathify "\\upleftharpoon")]
           [(#\⇂) (mathify "\\downrightharpoon")]
           [(#\⇃) (mathify "\\downleftharpoon")]
           ;; End Harpoons
           [(#\א) (mathify "\\aleph")]
           [(#\′) (mathify "\\prime")]
           [(#\∅) (mathify "\\varnothing")] ;;\\emptyset is too thin
           [(#\∇) (mathify "\\nabla")]
           [(#\♦) (mathify "\\diamondsuit")]
           [(#\♠) (mathify "\\spadesuit")]
           [(#\♣) (mathify "\\clubsuit")]
           [(#\♥) (mathify "\\heartsuit")]
           [(#\♯) (mathify "\\#")] ;;we want this more than we wand a sharp sign
           [(#\♭) (mathify "\\flat")]
           [(#\♮) (mathify "\\natural")]
           [(#\√) (mathify "\\surd")]
           [(#\¬) (mathify "\\neg")]
           [(#\△) (mathify "\\triangle")]
           [(#\∀) (mathify "\\forall")]
           [(#\∃) (mathify "\\exists")]
           [(#\∘) (mathify "\\circ")]
           [(#\θ) (mathify "\\theta")]
           [(#\τ) (mathify "\\tau")]
           [(#\υ) (mathify "\\upsilon")]
           [(#\φ) (mathify "\\phi")]
           [(#\δ) (mathify "\\delta")]
           [(#\ρ) (mathify "\\rho")]
           [(#\ε) (mathify "\\epsilon")]
           [(#\χ) (mathify "\\chi")]
           [(#\ψ) (mathify "\\psi")]
           [(#\ζ) (mathify "\\zeta")]
           [(#\ν) (mathify "\\nu")]
           [(#\ω) (mathify "\\omega")]
           [(#\η) (mathify "\\eta")]
           [(#\ι) (mathify "\\iota")]
           [(#\ξ) (mathify "\\xi")]
           [(#\Γ) (mathify "\\Gamma")]
           [(#\Ψ) (mathify "\\Psi")]
           [(#\∆) (mathify "\\Delta")]
           [(#\Δ) (mathify "\\Delta")] ;; these are different characters, I guess...
           [(#\Ξ) (mathify "\\Xi")]
           [(#\Υ) (mathify "\\Upsilon")]
           [(#\Ω) (mathify "\\Omega")]
           [(#\Θ) (mathify "\\Theta")]
           [(#\Π) (mathify "\\Pi")]
           [(#\Φ) (mathify "\\Phi")]
           [(#\±) (mathify "\\pm")]
           [(#\∩) (mathify "\\cap")]
           [(#\◇) (mathify "\\diamond")]
           [(#\⊕) (mathify "\\oplus")]
           [(#\⨁) (unsafe-mathify "\\bigoplus")]
           [(#\∓) (mathify "\\mp")]
           [(#\∪) (mathify "\\cup")]
           [(#\△) (mathify "\\bigtriangleup")]
           [(#\⊖) (mathify "\\ominus")]
           [(#\×) (mathify "\\times")]
           [(#\⊎) (mathify "\\uplus")]
           [(#\▽) (mathify "\\bigtriangledown")]
           [(#\⊗) (mathify "\\otimes")]
           [(#\÷) (mathify "\\div")]
           [(#\▹) (mathify "\\triangleright")]
           [(#\⊘) (mathify "\\oslash")]
           [(#\∗) (mathify "\\ast")]
           [(#\⊓) (mathify "\\sqcap")]
           [(#\⋂) (unsafe-mathify "\\bigcap")]
           [(#\∏) (unsafe-mathify "\\bigsqcap")]
           [(#\⊔) (mathify "\\sqcup")]
           [(#\⋃) (unsafe-mathify "\\bigcup")]
           [(#\∐) (unsafe-mathify "\\bigsqcup")]
           [(#\∨) (mathify "\\vee")]
           [(#\⋁) (unsafe-mathify "\\bigvee")]
           [(#\∧) (mathify "\\wedge")]
           [(#\⋀) (unsafe-mathify "\\bigwedge")]
           [(#\◃) (mathify "\\triangleright")]
           [(#\⊙) (mathify "\\odot")]
           [(#\★) (mathify "\\star")]
           [(#\†) (mathify "\\dagger")]
           [(#\•) (mathify "\\bullet")]
           [(#\‡) (mathify "\\ddagger")]
           [(#\≀) (mathify "\\wr")]
           [(#\⨿) (mathify "\\amalg")]
           [(#\≤) (mathify "\\leq")]
           [(#\≥) (mathify "\\geq")]
           [(#\≡) (mathify "\\equiv")]
           [(#\≟) (mathify "\\deceq")]
           [(#\≢) (mathify "\\nequiv")]
           [(#\⊨) (mathify "\\models")]
           [(#\≺) (mathify "\\prec")]
           [(#\≻) (mathify "\\succ")]
           [(#\∼) (mathify "\\sim")]
           [(#\⊥) (mathify "\\perp")]
           [(#\≼) (mathify "\\preceq")]
           [(#\≽) (mathify "\\succeq")]
           [(#\≃) (mathify "\\simeq")]
           [(#\≪) (mathify "\\ll")]
           [(#\≫) (mathify "\\gg")]
           [(#\⟪) (mathify "\\llangle")]
           [(#\⟫) (mathify "\\rrangle")]
           [(#\≍) (mathify "\\asymp")]
           [(#\∥) (mathify "\\parallel")]
           [(#\⊂) (mathify "\\subset")]
           [(#\⊃) (mathify "\\supset")]
           [(#\≈) (mathify "\\approx")]
           [(#\≜) (mathify "\\triangleq")]
           [(#\⋈) (mathify "\\bowtie")]
           [(#\⊆) (mathify "\\subseteq")]
           [(#\⊇) (mathify "\\supseteq")]
           [(#\≌) (mathify "\\cong")]
           [(#\⊏) (mathify "\\sqsubset")]
           [(#\⊐) (mathify "\\sqsupset")]
           [(#\≠) (mathify "\\neq")]
           [(#\⌣) (mathify "\\smile")]
           [(#\⊑) (mathify "\\sqsubseteq")]
           [(#\⊒) (mathify "\\sqsupseteq")]
           [(#\≐) (mathify "\\doteq")]
           [(#\⌢) (mathify "\\frown")]
           [(#\∅) (mathify "\\varnothing")]
           [(#\∈) (mathify "\\in")]
           [(#\∉) (mathify "\\notin")]
           [(#\∋) (mathify "\\ni")]
           [(#\∝) (mathify "\\propto")]
           [(#\⊢) (mathify "\\vdash")]
           [(#\⊣) (mathify "\\dashv")]
           [(#\⋆) (mathify "\\star")]
           [(#\☠) (mathify "\\skull")]
           [(#\☺) (mathify "\\smiley")]
           [(#\☻) (mathify "\\blacksmiley")]
           [(#\☹) (mathify "\\frownie")]
           [(#\à) "\\`{a}"]
           [(#\À) "\\`{A}"]
           [(#\á) "\\'{a}"]
           [(#\â) "\\^{a}"]
           [(#\Â) "\\^{A}"]
           [(#\Á) "\\'{A}"]
           [(#\ç) "\\c{c}"]
           [(#\Ç) "\\c{C}"]
           [(#\è) "\\`{e}"]
           [(#\È) "\\`{E}"]
           [(#\é) "\\'{e}"]
           [(#\É) "\\'{E}"]
           [(#\ê) "\\^{e}"]
           [(#\Ê) "\\^{E}"]
           [(#\í) "\\'{i}"]
           [(#\Í) "\\'{I}"]
           [(#\î) "\\^{i}"]
           [(#\Î) "\\^{I}"]
           [(#\ô) "\\^{o}"]
           [(#\Ô) "\\^{O}"]
           [(#\û) "\\^{u}"]
           [(#\Û) "\\^{U}"]
           [(#\ä) "\\\"a"]
           [(#\Ä) "\\\"A"]
           [(#\ü) "\\\"u"]
           [(#\Ü) "\\\"U"]
           [(#\ö) "\\\"o"]
           [(#\Ö) "\\\"O"]
           [(#\ø) "{\\o}"]
           [(#\Ø) "{\\O}"]
           [(#\ł) "{\\l}"]
           [(#\Ł) "{\\L}"]
           [(#\š) "{\\v s}"]
           [(#\uA7) "{\\S}"]
           [(#\…) "\\ldots "]
           [(#\⋮) "\\vdots "]
           [(#\𝔸) (mathify "{\\mathbb 𝔸}")]
           [(#\𝔹) (mathify "{\\mathbb 𝔹}")]
           [(#\𝔻) (mathify "{\\mathbb 𝔻}")]
           [(#\𝔼) (mathify "{\\mathbb 𝔼}")]
           [(#\𝔽) (mathify "{\\mathbb 𝔽}")]
           [(#\𝔾) (mathify "{\\mathbb 𝔾}")]
           [(#\ℍ) (mathify "{\\mathbb ℍ}")]
           [(#\𝕀) (mathify "{\\mathbb 𝕀}")]
           [(#\𝕁) (mathify "{\\mathbb 𝕁}")]
           [(#\𝕂) (mathify "{\\mathbb 𝕂}")]
           [(#\𝕃) (mathify "{\\mathbb 𝕃}")]
           [(#\𝕄) (mathify "{\\mathbb 𝕄}")]
           [(#\ℕ) (mathify "{\\mathbb N}")]
           [(#\𝕆) (mathify "{\\mathbb 𝕆}")]
           [(#\ℙ) (mathify "{\\mathbb ℙ}")]
           [(#\ℚ) (mathify "{\\mathbb ℚ}")]
           [(#\ℝ) (mathify "{\\mathbb ℝ}")]
           [(#\𝕊) (mathify "{\\mathbb 𝕊}")]
           [(#\𝕋) (mathify "{\\mathbb 𝕋}")]
           [(#\𝕌) (mathify "{\\mathbb 𝕌}")]
           [(#\𝕍) (mathify "{\\mathbb 𝕍}")]
           [(#\𝕎) (mathify "{\\mathbb 𝕎}")]
           [(#\𝕏) (mathify "{\\mathbb 𝕏}")]
           [(#\𝕐) (mathify "{\\mathbb 𝕐}")]
           [(#\ℤ) (mathify "{\\mathbb ℤ}")]
           [(#\𝒜) (mathify "{\\mathcal A}")]
           [(#\ℬ) (mathify "{\\mathcal B}")]
           [(#\𝒞) (mathify "{\\mathcal C}")]
           [(#\𝒟) (mathify "{\\mathcal D}")]
           [(#\ℰ) (mathify "{\\mathcal E}")]
           [(#\ℱ) (mathify "{\\mathcal F}")]
           [(#\𝒢) (mathify "{\\mathcal G}")]
           [(#\ℋ) (mathify "{\\mathcal H}")]
           [(#\ℐ) (mathify "{\\mathcal I}")]
           [(#\𝒥) (mathify "{\\mathcal J}")]
           [(#\𝒦) (mathify "{\\mathcal K}")]
           [(#\ℒ) (mathify "{\\mathcal L}")]
           [(#\ℳ) (mathify "{\\mathcal M}")]
           [(#\𝒩) (mathify "{\\mathcal N}")]
           [(#\𝒪) (mathify "{\\mathcal O}")]
           [(#\𝒫) (mathify "{\\mathcal P}")]
           [(#\𝒬) (mathify "{\\mathcal Q}")]
           [(#\ℛ) (mathify "{\\mathcal R}")]
           [(#\𝒮) (mathify "{\\mathcal S}")]
           [(#\𝒯) (mathify "{\\mathcal T}")]
           [(#\𝒰) (mathify "{\\mathcal U}")]
           [(#\𝒱) (mathify "{\\mathcal V}")]
           [(#\𝒳) (mathify "{\\mathcal X}")]
           [(#\𝒴) (mathify "{\\mathcal Y}")]
           [(#\𝒵) (mathify "{\\mathcal Z}")]
           [(#\₀) (mathify "_0")]
           [(#\₁) (mathify "_1")]
           [(#\₂) (mathify "_2")]
           [(#\₃) (mathify "_3")]
           [(#\₄) (mathify "_4")]
           [(#\₅) (mathify "_5")]
           [(#\₆) (mathify "_6")]
           [(#\₇) (mathify "_7")]
           [(#\₈) (mathify "_8")]
           [(#\₉) (mathify "_9")]
           [(#\〚) (mathify "[\\![")]
           [(#\〛) (mathify "]\\!]")]
           [(#\⊤) (mathify "\\top")]
           [(#\⊥) (mathify "\\bot")]
           [(#\ℓ) (mathify "\\ell")]
           [(#\¥) "{\\textyen}"]
           [(#\™) "{\\texttrademark}"]
           [(#\□) (mathify "\\square")]
           [(#\⊟) (mathify "\\boxminus")]
           [(#\⊩) (mathify "\\Vdash")]
           [(#\∖) (mathify "\\setminus")]
           [else (error (format "No rule for ~s" c))]))]))
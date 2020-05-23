# ユーフォニー指数の定義
## 自然言語らしさ
\\[E (\alpha, \beta, \gamma, \delta, \varepsilon) = 
\begin{cases}
    E_0 & (\varepsilon \not = 2) \\\
    -0.04{E_0}^2 + 1.4E_0 & (\varepsilon = 2) \\\
\end{cases} \\]
\\[ E_0 = \frac{1}{2} \cdot \left( 1 + \frac{1}{1 + \mathrm{exp}(0.5 \alpha - 7)} \right) \cdot \left( \frac{100}{1 + \mathrm{exp}(-2.26 \alpha - 0.0693 \beta + 0.0112 \gamma + 0.388 \delta - 11.9)} \right)\\]
\[
\begin{eqnarray*}
    \alpha & = & 単語長の平均 (文字数)\\\
    \beta & = & 語頭2字が両方子音になる割合 (\%)\\\
    \gamma & = & 語頭2字が両方子音で、かつ \mathrm{s-C-r/l/h} の構造を取らない割合 (\%)\\\
    \delta & = & 文章を通じての子音字の出現割合(\%)\\\
    \varepsilon & = & 子音クラスタをなす文字長の最頻値(文字数)
\end{eqnarray*}
\]
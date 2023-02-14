Script för att köra meta-analyser finns i "analysis/random-effects-model-REML-robust.R".


PARAMETRAR

I första delen av scriptet fylls följande parametrar i:
1. `data_file`: Namn för Excelfilen som skall användas (måste ligga i "data"-mappen).
2. `files_out_name`: Namn för excelfilen som ska sparas med resultat (hamnar i "output"-mappen).
3. `grouping`: Variabel som används för att grupera datan i. En separat model för respektive nivå kommer köras.
4. `exclude`: Används ifall någon nivå ska uteslutas från analysen.
5. `mods`: Variabler som ska användas som moderatorer.
6. `ci`: Konfidensintervall.


UTPUT

En excelfil sparas under "output"-mappen. Den innehåller deskriptiv information, test för respektive parameter, samt parvisa jämförelser för eventuella moderatorer.

Utöver det skapas en funnel-plot för respektive modell, samt en forest-plot. Dessa visas i RStudio, under "Plots"-fliken. Pilarna längst upp till vänster i fliken kan användas för att bläddra mellan dem. Dessa kan sparas med "Export"-knappen i fliken.
Zum Ausführen:
   cabal repl
oder kompilieren
   cabal install

Usage:
   ./orwell --repo "/path/to/your/repo" --period "1 week ago" -c
   
Optionen:
--repo - zu analysierendes Repo
--period - zu analysierende Commits (Git-Since-Syntax)
-c - analysiere Contribution (Tests hinzugefügt im Zeitraum, entwickelte Code-Zeilen, Feature+Bug-Commits ohne Tests)
-o - analysiere "Wem gehört welcher Test"   

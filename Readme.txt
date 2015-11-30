
Braucht mindestens den GHC 7.6

Abhängigkeiten:
cabal install shelly
cabal install pcre-light
cabal install time-exts
cabal install bloodhound

Zum Ausführen:
   cd src
   ghci Main.hs (im Interpreter)
oder kompilieren
   cd src
   ghc -o ../orwell -O Main.hs
   ./orwell --repo "/path/to/your/repo" --period "1 week ago" -c
   
Optionen:
--repo - zu analysierendes Repo
--period - zu analysierende Commits (Git-Since-Syntax)
-c - analysiere Contribution (Tests hinzugefügt im Zeitraum, entwickelte Code-Zeilen, Feature+Bug-Commits ohne Tests)
-o - analysiere "Wem gehört welcher Test"   
 
 


TODO: cabal-Datei für Abhängigkeiten dazulegen

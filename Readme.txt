
Braucht mindestens den GHC 7.6

Abhängigkeiten:
cabal install shelly
cabal install pcre-light

Zum Ausführen:
   cd src
   ghci Main.hs (im Interpreter)
oder kompilieren
   cd src
   ghc -o ../orwell -O Main.hs
   ./orwell --repo "/Users/mb/projekte/hunter" --period "1 week ago" -c
   
Optionen:
--repo - zu analysierendes Repo
--period - zu analysierende Commits (Git-Syntax)
-c - analysiere Contribution
-o - analyieren "Wem gehört welcher Test"   
 
 


TODO: cabal-Datei für Abhängigkeiten dazulegen
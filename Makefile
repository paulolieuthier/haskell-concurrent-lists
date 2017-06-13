clean:
	rm -f bin/*

LockFree: src/LockFree.hs
	ghc -threaded src/LockFree.hs -outputdir bin -o bin/LockFree
	./bin/LockFree +RTS -N

CoarseGrained: src/CoarseGrained.hs
	ghc -threaded src/CoarseGrained.hs -outputdir bin -o bin/CoarseGrained
	./bin/CoarseGrained +RTS -N

FineGrained: src/FineGrained.hs
	ghc -threaded src/FineGrained.hs -outputdir bin -o bin/FineGrained
	./bin/FineGrained +RTS -N

Optimistic: src/Optimistic.hs
	ghc -threaded src/Optimistic.hs -outputdir bin -o bin/Optimistic
	./bin/Optimistic +RTS -N

Lazy: src/Lazy.hs
	ghc -threaded src/Lazy.hs -outputdir bin -o bin/Lazy
	./bin/Lazy +RTS -N

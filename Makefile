clean:
	rm -f bin/*

LockFree: src/LockFree.hs
	ghc -threaded src/LockFree.hs -outputdir bin -o bin/LockFree
	./bin/LockFree +RTS -N

CoarseGrained: src/CoarseGrained.hs
	ghc -threaded src/CoarseGrained.hs -outputdir bin -o bin/CoarseGrained
	./bin/CoarseGrained +RTS -N

clean:
	rm -f bin/*

LockFree: src/LockFree.hs
	ghc src/LockFree.hs -outputdir bin -o bin/LockFree
	./bin/LockFree

CoarseGrained: src/CoarseGrained.hs
	ghc src/CoarseGrained.hs -outputdir bin -o bin/CoarseGrained
	./bin/CoarseGrained

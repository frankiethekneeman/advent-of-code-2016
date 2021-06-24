%: %.hxe
	./$@.hxe

.SECONDEXPANSION:
.SECONDARY:
%.hxe: %.hs shared/*.hs $$(@D)/*.hs $$(@D)/input
	ghc -o $@ -ishared/:$(@D) -outputdir=clutter $<

%/input: .cookie
	mkdir -p $$(dirname $@)
	curl https://adventofcode.com/2016/day/$*/input -H"Cookie: $$(cat .cookie)" > $@

clean:
	rm -rf clutter */*.hxe */input

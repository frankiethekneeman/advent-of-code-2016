%: %.hxe
	./$@.hxe

.SECONDEXPANSION:
.SECONDARY:
%.hxe: %.hs shared/*.hs $$(@D)/*.hs $$(@D)/input
	ghc -o $@ -ishared/:$(@D) -outputdir=clutter $< -main-is $$([ $(@F) = 2.hxe ] && echo Two || echo One)

%/input: .cookie
	mkdir -p $$(dirname $@)
	curl https://adventofcode.com/2016/day/$*/input -H"Cookie: $$(cat .cookie)" > $@

clean:
	rm -rf clutter */*.hxe */input

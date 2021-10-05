HOPTS=-XUndecidableInstances -XFlexibleInstances -XTypeSynonymInstances -XMultiParamTypeClasses -XFunctionalDependencies -XTypeFamilies

%: %.hxe
	./$@.hxe

.SECONDEXPANSION:
.SECONDARY:
%.hxe: %.hs shared/*.hs $$(@D)/*.hs $$(@D)/input
	ghc -o $@ -ishared/:$(@D) $(HOPTS) -outputdir=clutter $< -main-is $$([ $(@F) = 2.hxe ] && echo Two || echo One)

%/input: .cookie
	mkdir -p $$(dirname $@)
	curl https://adventofcode.com/2016/day/$*/input -H"Cookie: $$(cat .cookie)" > $@

clean:
	rm -rf clutter */*.hxe */input

all: $(foreach day,$(shell seq 1 24),$(foreach part,1 2,$(day)/$(part))) 25/1

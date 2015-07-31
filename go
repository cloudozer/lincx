#!/usr/bin/env sh

./lincx.img -eval \
	"lists:map(fun application:start/1, [crypto,asn1,public_key,ssh,compiler,syntax_tools,xmerl,mnesia,lager,linc])" \
	-config /lincx/priv/sys.config -home /lincx \
	-pz /lincx/apps/linc/ebin /lincx/apps/linc_max/ebin /lincx/apps/linc_us3/ebin \
		/lincx/apps/linc_us4/ebin /lincx/deps/eenum/ebin /lincx/deps/enetconf/ebin \
		/lincx/deps/lager/ebin /lincx/deps/meck/ebin /lincx/deps/of_config/ebin \
		/lincx/deps/of_protocol/ebin /lincx/deps/pkt/ebin /lincx/priv

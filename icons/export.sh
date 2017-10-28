#!/usr/bin/env bash

# find -path "./**/production/ic_close_24px.svg" -exec cp {} ~/.emacs.d/lisp/fstar.el/icons/ \;
# find -path "./**/production/ic_create_24px.svg" -exec cp {} ~/.emacs.d/lisp/fstar.el/icons/ \;
# find -path "./**/production/ic_description_24px.svg" -exec cp {} ~/.emacs.d/lisp/fstar.el/icons/ \;
# find -path "./**/production/ic_find_in_page_24px.svg" -exec cp {} ~/.emacs.d/lisp/fstar.el/icons/ \;
# find -path "./**/production/ic_fingerprint_24px.svg" -exec cp {} ~/.emacs.d/lisp/fstar.el/icons/ \;
# find -path "./**/production/ic_replay_24px.svg" -exec cp {} ~/.emacs.d/lisp/fstar.el/icons/ \;
# find -path "./**/production/ic_search_24px.svg" -exec cp {} ~/.emacs.d/lisp/fstar.el/icons/ \;
# find -path "./**/production/ic_settings_24px.svg" -exec cp {} ~/.emacs.d/lisp/fstar.el/icons/ \;
# find -path "./**/production/ic_stop_24px.svg" -exec cp {} ~/.emacs.d/lisp/fstar.el/icons/ \;
# find -path "./**/production/ic_toc_24px.svg" -exec cp {} ~/.emacs.d/lisp/fstar.el/icons/ \;
# find -path "./**/production/ic_tune_24px.svg" -exec cp {} ~/.emacs.d/lisp/fstar.el/icons/ \;
# find -path "./**/production/ic_bug_report_24px.svg" -exec cp {} ~/.emacs.d/lisp/fstar.el/icons/ \;
# find -path "./**/production/ic_content_copy_24px.svg" -exec cp {} ~/.emacs.d/lisp/fstar.el/icons/ \;
# find -path "./**/production/ic_done_24px.svg" -exec cp {} ~/.emacs.d/lisp/fstar.el/icons/ \;

cp ic_close_24px.svg ../etc/icons/quit-windows.svg
cp ic_code_24px.svg ../etc/icons/switch-to-implementation.svg
cp ic_create_24px.svg ../etc/icons/switch-to-rst.svg
cp ic_description_24px.svg ../etc/icons/lookup-definition.svg
cp ic_find_in_page_24px.svg ../etc/icons/lookup-documentation.svg
cp ic_fingerprint_24px.svg ../etc/icons/switch-to-interface.svg
cp ic_replay_24px.svg ../etc/icons/reload.svg
cp ic_search_24px.svg ../etc/icons/search.svg
cp ic_settings_24px.svg ../etc/icons/settings.svg
cp ic_stop_24px.svg ../etc/icons/stop.svg
cp ic_toc_24px.svg ../etc/icons/outline.svg
cp ic_tune_24px.svg ../etc/icons/list-options.svg
cp ic_visibility_24px.svg ../etc/icons/quick-peek.svg

cp ic_vertical_align_bottom_bold_24px.svg ../etc/icons/goto-end.svg
cp ic_vertical_align_center_bold_24px.svg ../etc/icons/goto-point.svg
cp ic_content_copy_bug_24px.svg ../etc/icons/copy-error-message.svg
cp ic_vertical_align_bottom_up_no_bottom_bold_24px.svg ../etc/icons/previous.svg
cp ic_vertical_align_bottom_no_bottom_bold_24px.svg ../etc/icons/next.svg
cp ic_vertical_align_bottom_white_no_bottom_24px.svg ../etc/icons/next-lax.svg
cp ic_vertical_align_bottom_white_24px.svg ../etc/icons/goto-end-lax.svg
cp ic_vertical_align_center_white_bold_24px.svg ../etc/icons/goto-point-lax.svg

# cp equal_sign_24px.svg ../etc/icons/eval.svg
cp verify_cli_24px.svg ../etc/icons/cli-verify.svg
cp calculator_cut_24px.svg ../etc/icons/eval.svg
cp ghost_24px.svg ../etc/icons/ghost.svg

cd ../etc/icons/
for shade in light dark; do
    mkdir -p "$shade"
    rm "$shade"/*.png "$shade"/*.xpm
    for svg in *.svg; do
        echo "$svg"
        icon="${svg%.svg}"
        shaded="$shade/$icon"
        ../../icons/recolor.py "$shade" "$icon.svg" "$shaded.svg"
        rsvg -w 24 -h 24 "$shaded.svg" "$shaded.png"
        convert "$shaded.png" "$shaded.xpm"
    done
done

# get rid of pre-recoloring templates
rm ./*.svg

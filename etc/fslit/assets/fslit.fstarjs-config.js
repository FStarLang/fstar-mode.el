/* global $ FSLit FStar __FSTAR_JS_CURRENT_FST_FILE_NAME__ */
$(function() {
    FSLit.StandaloneClient = FStar.CLI.Client;
    FStar.CLI.WORKER_DIRECTORY = "_static/fstar.js/";
    FStar.IDE.WORKER_DIRECTORY = "_static/fstar.js/";
    FStar.IDE.LiterateClient.run(__FSTAR_JS_CURRENT_FST_FILE_NAME__);
});

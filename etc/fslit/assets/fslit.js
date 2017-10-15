"use strict";
/* global window FSLit $ CodeMirror */

window.FSLit = window.FSLit || {};

(function () {
    var HTML = ['<div class="fstar-remote-editor">',
                '  <div class="editor"></div>',
                '  <div class="control-panel">',
                '    <input class="run" type="button" value="" disabled="disabled" />',
                '  </div>',
                '  <pre class="stdout"></pre>',
                '</div>'].join("\n");

    var StandaloneClient = FSLit.StandaloneClient = function(root) {
        var $root = this.$root = root ? $(root) : $(HTML);

        this.$editor = $root.find(".editor");
        this.$stdout = $root.find(".stdout").empty();
        this.$run = $root.find(".run").click($.proxy(this.verifyCurrentInput, this));

        this.toggleButton(false);
        this.editor = new CodeMirror(this.$editor[0], { lineNumbers: true,
                                                        theme: "tango",
                                                        mode: "text/x-fstar" });
    };

    StandaloneClient.prototype.toggleButton = function(disabled, message) {
        this.$run.prop("disabled", disabled);
        this.$run.val(message || "Run F*!");
    };

    StandaloneClient.prototype.verify = function(input) {
        $.post("http://www.rise4fun.com/rest/ask/fstar", input, function (data) {
            this.$stdout.text(data);
            this.toggleButton(false);
        });
    };

    StandaloneClient.prototype.verifyCurrentInput = function(_event) {
        var fcontents = this.editor.getValue();
        this.$stdout.empty();
        this.toggleButton(true, "Running…");
        this.verify(fcontents);
    };

    StandaloneClient.prototype.loadDocumentAsync = function(documentURL) {
        var editor = this.editor;
        $.get(documentURL, function(data) {
            editor.setValue(data);
        }, 'text');
    };

    StandaloneClient.prototype.setValue = function(fcontents) {
        this.editor.setValue(fcontents, -1);
    };

    StandaloneClient.prototype.setFilename = function() {};

    function openStandaloneEditor(documentURL, $linkNode) {
        var client = new FSLit.StandaloneClient(); // This can be overwritten
        client.setFilename(documentURL.replace(/^.*\//, ""));
        $.get(documentURL, function(data) { client.setValue(data); }, 'text');
        $linkNode.parent().after(client.$root);
        $linkNode.remove();
    }

    function addStandaloneEditorLinks() {
        $(".fstar-standalone-editor-link")
            .each(function () {
                var href = $(this).attr("href");
                var $span = $('<span class="fstar-standalone-editor-link">');
                $span.text($(this).text());
                $span.click(function() { openStandaloneEditor(href, $span); });
                $(this).replaceWith($span);
            });
    }

    function activateSolutionBodies() {
        $(".solution-body")
            .each(function () {
                var body = $(this);
                body.click(function () {
                    body.addClass("fstar-clear-solution");
                });
            });
    }

    $(function() {
        addStandaloneEditorLinks();
        activateSolutionBodies();
    });
}());

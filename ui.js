function edit_text($parent, $text, $editBtn, text_change_fn) {
    var $input = $('<input>');
    $input.val($text.text());

    function submit_sensor_name() {
        var new_name = $input.val();
        $text.text(new_name);
        $input.remove();
        $text.show();
        $editBtn.show();
        text_change_fn(new_name);
    }

    $input.focusout(submit_sensor_name);
    $input.keypress(function (evt) {
        evt = (evt) ? evt : (window.event) ? event : null;
        if (evt) {
            var charCode = (evt.charCode) ? evt.charCode : ((evt.keyCode) ? evt.keyCode : ((evt.which) ? evt.which : 0));
            if (charCode == 13)
                $input.focusout();
        }
    })
    $text.hide();
    $editBtn.hide();
    $input.appendTo($parent);
}

function add_sensor_name(uuid, name, sensor_change_evt) {
    var $parent = $("#" + uuid + ' .sensor-name-cell');
    var $parent2 = $("#" + uuid + ' .sensor-name-edit-cell');

    $('<span/>')
        .addClass("sensor-name")
        .text(name)
        .appendTo($parent);

    var $btn =
        $('<button/>')
            .addClass("edit-btn")
            .text("Edit")
            .click(function () {
                var $editBtn = $('#' + uuid + " .edit-btn");
                var $text = $('#' + uuid + " .sensor-name");
                var $parent = $("#" + uuid + ' .sensor-name-cell');
                edit_text($parent, $text, $editBtn, sensor_change_evt)
            });
    $btn.appendTo($parent2);
}


function set_status_active($btn, is_active){
    if(is_active){
        $btn.text("Deactivate");
    } else {
        $btn.text("Activate");
    }
}

function is_status_active($btn) {
    return $btn.text() == "Deactivate";
}

function set_status_active_uuid(uuid, is_active){
    var $btn = $("#" + uuid + " .activate-btn");
    set_status_active($btn, is_active);
}

function add_activate_btn(uuid, is_active, change_activation_fn) {
    var $parent = $("#" + uuid + ' .sensor-activate-cell');

    var $btn =
        $('<button/>')
            .addClass("activate-btn")
            .text("Activate")
            .click(function () {
                var active = is_status_active($btn);
                change_activation_fn(active);
                set_status_active($btn, !active);
            });
    set_status_active($btn, is_active);
    $btn.appendTo($parent);
}




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

function add_sensor_name(sensor_id, name, sensor_change_evt) {
    var $parent = $("#" + sensor_id + ' .sensor-name-cell');
    var $parent2 = $("#" + sensor_id + ' .sensor-name-edit-cell');

    $('<span/>')
        .addClass("sensor-name")
        .text(name)
        .appendTo($parent);

    var $btn =
        $('<button/>')
            .addClass("edit-btn")
            .text("Edit")
            .click(function () {
                var $editBtn = $('#' + sensor_id + " .edit-btn");
                var $text = $('#' + sensor_id + " .sensor-name");
                var $parent = $("#" + sensor_id + ' .sensor-name-cell');
                edit_text($parent, $text, $editBtn, sensor_change_evt)
            });
    $btn.appendTo($parent2);
}



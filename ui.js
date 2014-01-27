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
            var charCode =
                evt.charCode ? evt.charCode :
                (evt.keyCode ? evt.keyCode :
                 (evt.which ? evt.which : 0));
            if (charCode == 13)
                submit_sensor_name();
        }
    })
    $text.hide();
    $editBtn.hide();
    $input.appendTo($parent);
}

function add_sensor_name(uuid, name, sensor_change_evt) {
    var $parent = $("#" + uuid + ' .sensor-name-cell');

    $('<span/>')
        .addClass("sensor-name")
        .text(name)
        .appendTo($parent);

    var $btn =
        $("<button><i class='fa fa-pencil'></i></button>")
            .addClass("edit-btn btn-s")
            .click(function () {
                var $editBtn = $('#' + uuid + " .edit-btn");
                var $text = $('#' + uuid + " .sensor-name");
                var $parent = $("#" + uuid + ' .sensor-name-cell');
                edit_text($parent, $text, $editBtn, sensor_change_evt)
            });
    $btn.appendTo($parent);
}


function set_status_active(uuid, is_active) {
    var $icon = $("#" + uuid + " .activate-btn i");
    if (is_active) {
        $icon.attr('class', 'fa fa-check-circle-o');
    } else {
        $icon.attr('class', 'fa fa-circle-o');
    }
    $("#" + uuid + " .activate-btn").attr('data-active', is_active);
}

function add_activate_btn(uuid, is_active, change_activation_fn) {
    var $parent = $("#" + uuid + ' .sensor-activate-cell');
    var $btn =
        $('<button/>')
            .addClass("activate-btn")
            .addClass("btn btn-sm btn-primary")
            .attr("data-active", 'false')
            .append($('<i/>'))
            .click(function () {
                var active = $btn.attr("data-active") == 'true';
                change_activation_fn(active);
                set_status_active(uuid, !active);
            });
    set_status_active(uuid, is_active);
    $btn.appendTo($parent);
}

function add_sensor_row(uuid, sensor_name) {
    var row = $("<tr></tr>", {
        id: uuid,
        class: 'sensor',
    });
    row.append($("<td class='sensor-name-cell' />"));
    row.append($("<td class='sensor-activate-cell' />"));
    row.append($("<td/>")
               .append($("<span class='sample-count'>unknown</span>"))
               .append($("<button class='btn btn-sm btn-primary plot-btn' />")
                       .append($("<i class='fa fa-bar-chart-o'></i>"))
                       .click(function() {
                           $.ajax('/devices/'+uuid+'/samples/json',
                                  { success: function(data, error, xhr) {
                                      filtered = [];
                                      for (i in data) {
                                          if (data[i].sensor == 1)
                                              filtered.push(data[i]);
                                      }
                                      curve_set_data(filtered);
                                  }
                                  }
                                 )
                           })
                       )
               .append($("<button class='btn btn-sm btn-primary download-btn'/>")
                       .append($("<i class='fa fa-download'></i>"))
                       .click(function() {
                           location.href = "/devices/" + uuid + "/samples/csv";
                       })
                      )

               .append($("<button class='btn btn-sm btn-danger delete-btn'/>")
                       .append($("<i class='fa fa-trash-o'></i>"))
                       )
              );
    row.append($("<td>2 min</td>"));
    row.append($("<td/>")
               .append($("Ok"))
               .append($("<button class='btn btn-primary btn-sm configure-btn'>Configure</button>"))
               );

    $("#sensors tbody").append(row);
    add_sensor_name(uuid, sensor_name, function(name) {
        sensor_name_change(uuid, name);
    });

    add_activate_btn(uuid, false, function(is_active) {
        start_stop_acquire(uuid, is_active);
    });
}

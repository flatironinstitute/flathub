System.register(["highcharts", "highcharts/modules/exporting"], function (exports_1, context_1) {
    "use strict";
    var __moduleName = context_1 && context_1.id;
    function assert(x) {
        if (!x)
            throw new Error("assertion failure");
        return x;
    }
    exports_1("assert", assert);
    function fill_select_terms(s, f, a) {
        var def = document.createElement('option');
        def.text = 'all';
        s.add(def);
        a.buckets.sort(function (c, d) { return -(c.key < d.key) || +(c.key > d.key); });
        for (var _i = 0, _a = a.buckets; _i < _a.length; _i++) {
            var b = _a[_i];
            var opt = document.createElement('option');
            opt.value = b.key;
            if (f.enum && b.key in f.enum)
                opt.text = f.enum[b.key];
            else
                opt.text = b.key;
            opt.text += ' (' + b.doc_count + ')';
            s.add(opt);
        }
        s.disabled = false;
    }
    exports_1("fill_select_terms", fill_select_terms);
    function field_option(f) {
        var o = document.createElement('option');
        o.value = f.name;
        o.text = f.title;
        if (f.descr)
            o.title = f.descr;
        return o;
    }
    exports_1("field_option", field_option);
    function updateMathJax() {
        var MathJax = window.MathJax;
        if (MathJax)
            setTimeout(function () {
                if (!MathJax.Hub.queue.pending)
                    MathJax.Hub.Queue(["Typeset", MathJax.Hub]);
            });
    }
    exports_1("updateMathJax", updateMathJax);
    function field_title(field, rmf) {
        var h = document.createElement('span');
        h.textContent = field.title;
        if (rmf) {
            var rm = document.createElement('button');
            rm.className = 'remove';
            rm.innerHTML = '&times;';
            rm.onclick = rmf;
            h.insertBefore(rm, h.firstChild);
        }
        if (field.units) {
            var units = document.createElement('span');
            units.className = 'units';
            units.appendChild(document.createTextNode(field.units));
            h.appendChild(units);
        }
        if (field.descr) {
            h.className = 'tooltip';
            var tt = document.createElement('span');
            tt.className = 'tooltiptext';
            tt.innerHTML = field.descr;
            h.appendChild(tt);
        }
        updateMathJax();
        return h;
    }
    exports_1("field_title", field_title);
    function render_funct(field) {
        if (field.base === 'f')
            return function (data) { return data != undefined ? parseFloat(data).toPrecision(8) : data; };
        if (field.enum) {
            var e_1 = field.enum;
            return function (data) { return data in e_1 ? e_1[data] : data; };
        }
        return function (data) { return data; };
    }
    exports_1("render_funct", render_funct);
    function toggle_log(chart) {
        var axis = chart.get('tog');
        if (axis.userOptions.type !== 'linear') {
            axis.update({
                min: 0,
                type: 'linear'
            });
        }
        else {
            axis.update({
                min: null,
                type: 'logarithmic'
            });
        }
    }
    exports_1("toggle_log", toggle_log);
    function axis_title(f) {
        return {
            // useHTML: true, // not enough for mathjax
            text: f.title + (f.units ? ' [' + f.units + ']' : '')
        };
    }
    exports_1("axis_title", axis_title);
    function histogram_options(f) {
        var render = render_funct(f);
        return {
            chart: {
                animation: false,
                zoomType: 'x',
            },
            legend: {
                enabled: false
            },
            title: {
                text: null
            },
            credits: {
                enabled: false
            },
            xAxis: {
                type: 'linear',
                title: axis_title(f),
                gridLineWidth: 1,
            },
            yAxis: {
                id: 'tog',
                type: 'linear',
                title: { text: 'Count' },
                min: 0,
            },
            tooltip: {
                animation: false,
                formatter: function () {
                    return '[' + render(this.x) + ',' + render(this.x + this.series.options.pointInterval) + '): ' + this.y;
                }
            },
            exporting: {
                enabled: true
            },
            plotOptions: {
                column: {
                    grouping: false,
                    groupPadding: 0,
                    pointPadding: 0,
                    borderWidth: 0,
                    shadow: false,
                    pointPlacement: 0.5,
                    animation: { duration: 0 },
                    states: {
                        hover: {
                            enabled: false
                        }
                    }
                }
            }
        };
    }
    exports_1("histogram_options", histogram_options);
    var highcharts_1, exporting_1;
    return {
        setters: [
            function (highcharts_1_1) {
                highcharts_1 = highcharts_1_1;
            },
            function (exporting_1_1) {
                exporting_1 = exporting_1_1;
            }
        ],
        execute: function () {
            exporting_1.default(highcharts_1.default);
            window.toggleDisplay = function toggleDisplay(ele) {
                $('#' + ele).toggle();
            };
        }
    };
});
//# sourceMappingURL=common.js.map
function logRequestResult(good, bad) {
    function then(response) {
        var log = document.getElementById('log');
        var old_content = log.innerText;
        if (response.status == 200) {
            log.innerText = good + "\n" + old_content;
        } else {
            log.innerText = bad + "\n" + old_content;
        }
    }
    return then;
}

function fetchAndReport(url, opts, good, bad) {
    fetch(url, opts).then(logRequestResult(good, bad));
}

function start() {
    fetchAndReport(
        "/esi/mnesia_bench_esi:start", {
            body: document.getElementById('ctllocation').value,
            method: "POST",
        }, "mnesia started", "could not start mnesia"
    );
}

function stop() {
    fetchAndReport(
        "/esi/mnesia_bench_esi:stop", {
            body: document.getElementById('ctllocation').value,
            method: "POST",
        }, "mnesia stopped", "could not stop mnesia"
    );
}

function reset() {
    fetchAndReport(
        "/esi/mnesia_bench_esi:reset", {
            body: document.getElementById('ctllocation').value,
            method: "POST",
        }, "reset mnesia", "failed to reset mnesia"
    );
}

function createSchema() {
    fetchAndReport(
        "/esi/mnesia_bench_esi:create_schema", {
            body: document.getElementById('ctllocation').value,
            method: "POST",
        }, "created schema", "failed to create schema"
    );
}

function mktab() {
    fetchAndReport(
        "/esi/mnesia_bench_esi:create_table", {
            body: (
                document.getElementById('tablocation').value
                + ","
                + document.getElementById('tabtype').value
            ),
            method: "POST",
        }, "created table", "failed to create table"
    );
}

function writeTab() {
    fetchAndReport(
        "/esi/mnesia_bench_esi:write_table", {
            body: document.getElementById('tabrecords').value,
            method: "POST",
        }, "wrote records", "failed to write records"
    );
}

function startStressers() {
    fetchAndReport(
        "/esi/mnesia_bench_esi:start_workers", {
            body: (
                document.getElementById('stressprocs').value
                + ","
                + document.getElementById('stressmode').value
            ),
            method: "POST",
        }, "started stressers", "failed to start stressers"
    );
}

function killStressers() {
    fetchAndReport(
        "/esi/mnesia_bench_esi:stop_workers", {method: "POST"},
        "stopped stressers", "failed to stop stressers"
    );
}

function modTableCopies() {
    fetchAndReport(
        "/esi/mnesia_bench_esi:mod_table_copies", {
            method: "POST",
            body: (
                document.getElementById('tabmodkind').value
                + ","
                + document.getElementById('tabmodtab').value
                + ","
                + document.getElementById('tabmodtype').value
                + ","
                + document.getElementById('tabmodnode').value
            ),
        },
        "modified table copies", "failed to modify table copies"
    );
}

function backup() {
    fetchAndReport(
        "/esi/mnesia_bench_esi:backup", { method: "POST", },
        "backed up db", "failed to back up db"
    );
}

function dumpTables() {
    fetchAndReport(
        "/esi/mnesia_bench_esi:dump_tables", { method: "POST", },
        "dumped ram tabs", "failed to dump ram tabs"
    );
}


      
var transactions_options = {
  chart: {
      height: 350,
      type: 'line',
      toolbar: { show: false },
  },
  dataLabels: {
      enabled: false
  },
  series: [],
  title: {
      text: 'Transactions per second',
  },
  noData: {
    text: 'Loading...'
  },
    xaxis: { 
        labels: { show: false }
    }
};

var transactions = new ApexCharts(document.getElementById("transactions"), transactions_options);
transactions.render();
var transactions_last = {
    commits: 0,
    failures: 0,
    restarts: 0,
    log_writes: 0
}
var transactions_hist = {
    commits: [],
    failures: [],
    restarts: [],
    log_writes: [],
};

var latency_options = {
  chart: {
      height: 350,
      type: 'area',
      toolbar: { show: false },
  },
  dataLabels: {
      enabled: false
  },
  series: [],
  title: {
      text: 'Average latency for operations in milliseconds',
  },
  noData: {
    text: 'Loading...'
  },
    xaxis: { 
        labels: { show: false }
    },
};
var latency = new ApexCharts(document.getElementById("latency"), latency_options);
latency.render();
var latency_hist_read = [];
var latency_hist_write = [];


window.setInterval(function () {
    fetch("/esi/mnesia_bench_esi:transactions").then(
        (response) => { return response.text() }
    ).then((content) => {
        let [commraw, failraw, restartraw, logwriteraw] = content.split(',');
        let now = {
            commits: parseInt(commraw, 10),
            failures: parseInt(failraw, 10),
            restarts: parseInt(restartraw, 10),
            log_writes: parseInt(logwriteraw, 10),
        };
        let series = []
        for (key in now) {
            let count_now = now[key];
            let delta = count_now - transactions_last[key];
            if (delta >= 0 && delta <= 500000) {
                transactions_hist[key].push(delta);
            }
            transactions_last[key] = count_now;
            if (transactions_hist[key].length >= 50) {
                transactions_hist[key].shift();
            }

            series.push({name: key, data: transactions_hist[key]});
        }
        transactions.updateSeries(series);
    })},
    1000
);

window.setInterval(function () {
    fetch("/esi/mnesia_bench_esi:latency").then(
        (response) => { return response.text() }
    ).then((content) => {
        let [read_raw, write_raw] = content.split(',');
        let read = Math.trunc(parseFloat(read_raw));
        let write = Math.trunc(parseFloat(write_raw));
        if (read >= 0 && write >= 0) {
            latency_hist_read.push(read);
            latency_hist_write.push(write);
            latency.updateSeries([
                {name: "read", data: latency_hist_read},
                {name: "write", data: latency_hist_write},
            ]);
        }
        if (latency_hist_read.length >= 50) {
            latency_hist_read.shift();
        }
        if (latency_hist_write.length >= 50) {
            latency_hist_write.shift();
        }
    })},
    1000
);

window.setInterval(function () {
    fetch("/esi/mnesia_bench_esi:erlang_cluster_nodes").then(
        (response) => { return response.text() }
    ).then((content) => {
        document.getElementById('nodes').innerText = content;
    })},
    1000
);


var tabsize_options = {
  chart: {
      height: 350,
      type: 'area',
      toolbar: { show: false },
  },
  dataLabels: {
      enabled: false
  },
  series: [],
  title: {
      text: 'Table items',
  },
  noData: {
    text: 'Loading...'
  }
};
var tabsize = new ApexCharts(document.getElementById("tabsize"), tabsize_options);
tabsize.render();
var tabsize_hist = {};


window.setInterval(function () {
    fetch("/esi/mnesia_bench_esi:tabsize").then(
        (response) => { return response.text() }
    ).then((content) => {
        let lines = content.split('\n');
        let series = []
        for (let i = 0; i < lines.length; i++) {
            let [name, items] = lines[i].split(',');
            if (name != "") {
                now = parseInt(items, 10);
                if (name in tabsize_hist) {
                    tabsize_hist[name].push(now);
                } else {
                    tabsize_hist[name] = [now];
                }
                if (tabsize_hist[name].length >= 50) {
                    tabsize_hist[name].shift();
                }
                series.push({name: name, data: tabsize_hist[name]});
            }
        }
        tabsize.updateSeries(series);
    })},
    2000
);

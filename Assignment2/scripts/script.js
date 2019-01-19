$(document).ready(function () {
    // Declare variabels
    var width = 1000;
    var height = 480;

    var projection = d3.geoEquirectangular()
    var path = d3.geoPath().projection(projection);
    var svg = d3.select('body')
        .append('svg')
        .attr('width', width)
        .attr('height', height)

    svg.append('rect').attr('width', width).attr('height', height).attr('fill', 'white');
    // Group svg together
    var g = svg.append('g');
    // Draw map in canvas
    d3.json('https://d3js.org/world-50m.v1.json', function (error, data) {
        if (error) console.log(error);

        g.append('path')
            .datum(topojson.feature(data, data.objects.countries))
            .attr('d', path);
        // Include naval dataset
        d3.json('../json/Naval-disaster-data.json', function (error, data) {
            // If evrything is loaded create variable
            var locations = data.features;
            // Select all circkels
            g.selectAll('circle')
                .data(locations)
                .enter()
                .append('circle')
                // Set lattitude en longtitude
                .attr('cx', function (d) {
                    if (d.geometry) {
                        // Longtitude
                        return projection([d.geometry.coordinates[0], d.geometry.coordinates[1]])[0];
                    }
                })
                .attr('cy', function (d) {
                    if (d.geometry) {
                        // Lattitude
                        return projection([d.geometry.coordinates[0], d.geometry.coordinates[1]])[1];
                    }
                })
                // Adjust radius with amount of death by ship
                .attr('r', function (d) {
                    if (d.properties.death) {
                        // Scale radius for beter readability.
                        return Math.pow(parseInt(d.properties.death), 1 / 4);
                    }
                })
                // Add color to circkel
                .style('fill', 'red')
                // Show more details on hover
                .on('mouseover', function (d) {
                    // Select data of selected circle
                    d3.select(this).style('fill', 'black');
                    d3.select('#name').text(d.properties.name);
                    d3.select('#navy').text(d.properties.navy);
                    d3.select('#date_sunk').text(d.properties.date_sunk);
                    d3.select('#death').text(d.properties.death);
                    // Add styling to tooltip
                    d3.select('#tooltip')
                        .style('left', (d3.event.pageX + 20) + 'px')
                        .style('top', (d3.event.pageY - 80) + 'px')
                        .style('display', 'block')
                        .style('opacity', 0.8)
                })
                // Reset circle when hover is finished.
                .on('mouseout', function (d) {
                    d3.select(this).style('fill', 'red');
                    // Show tooltip to none
                    d3.select('#tooltip')
                        .style('display', 'none');
                });
        });
    });
});

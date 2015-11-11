'use strict';

var gulp = require('gulp');
var $ = require('gulp-load-plugins')();

gulp.task('release', function(){
     gulp.src('./modules/swagger-codegen-cli/target/swagger-codegen-cli.jar')
          .pipe($.githubRelease({
               repo: 'swagger-codegen',
               owner: '28msec',
               manifest: require('./package.json')
          }));
});

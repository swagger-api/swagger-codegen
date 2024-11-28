DO NOT EDIT - See: https://www.eclipse.org/jetty/documentation/current/startup-modules.html

[description]
Provides the logback core implementation
and logback-access

[tags]
logging
internal

[depends]
resources

[files]
maven://ch.qos.logback/logback-core/${logback.version}|lib/logback/logback-core-${logback.version}.jar
basehome:modules/logback-impl

[lib]
lib/logback/logback-core-${logback.version}.jar

[license]
Logback: the reliable, generic, fast and flexible logging framework.
Copyright (C) 1999-2012, QOS.ch. All rights reserved. 

This program and the accompanying materials are dual-licensed under
either:

    the terms of the Eclipse Public License v1.0 
    as published by the Eclipse Foundation: 
    http://www.eclipse.org/legal/epl-v10.html
 
or (per the licensee's choosing) under
    
    the terms of the GNU Lesser General Public License version 2.1
    as published by the Free Software Foundation: 
    http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html

[ini]
logback.version?=1.1.7
jetty.webapp.addServerClasses+=,${jetty.base.uri}/lib/logback/

plugins {
    kotlin("jvm") version "2.1.10"
    java
}

group = "ru.ifmo.pds"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
}

dependencies {
    implementation("ch.qos.logback:logback-classic:1.2.10")
    testImplementation(kotlin("test-junit"))
}

sourceSets.main {
    java.srcDir("src")
}

sourceSets.test {
    java.srcDir("test")
}

val processId = project.properties["processId"] as? String ?: "1"
val implName = project.properties["implName"] as? String ?: "ProcessImpl"

java {
    toolchain {
        languageVersion.set(JavaLanguageVersion.of(21))
    }
}

tasks {
    test {
        testLogging.showStandardStreams = true
        systemProperty("implName", implName)
        project.properties["skipCountCheck"]?.let { systemProperty("skipCountCheck", it) }
    }

    register<JavaExec>("node") {
        classpath = sourceSets["main"].runtimeClasspath
        mainClass.set("mutex.system.NodeKt")
        args = listOf(processId, implName)
        standardInput = System.`in`
    }

    register<JavaExec>("system") {
        classpath = sourceSets["main"].runtimeClasspath
        mainClass.set("mutex.system.SystemKt")
        args = listOf(implName)
        standardInput = System.`in`
    }
}

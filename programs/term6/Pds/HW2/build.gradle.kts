plugins {
    kotlin("jvm") version "2.1.10"
    java
}

repositories {
    mavenCentral()
}

dependencies {
    testImplementation("org.junit.jupiter:junit-jupiter-api:5.9.0")
    testRuntimeOnly("org.junit.jupiter:junit-jupiter-engine:5.9.0")
    implementation("org.slf4j:slf4j-api:1.7.36")
    implementation("org.slf4j:slf4j-simple:1.7.36")
}

tasks.test {
    useJUnitPlatform()
    filter {
        includeTestsMatching("solution.*")
    }
}

sourceSets.main {
    java.setSrcDirs(listOf("src"))
    resources.setSrcDirs(listOf("resources"))
}

sourceSets.test {
    java.setSrcDirs(listOf("test"))
}

kotlin {
    jvmToolchain(21)
}

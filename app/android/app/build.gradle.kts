import java.io.File

plugins {
    alias(libs.plugins.android.application)
    alias(libs.plugins.kotlin.android)
    alias(libs.plugins.kotlin.compose)
}

// ── Paths ─────────────────────────────────────────────────────────────────────

val repoRoot = rootProject.projectDir.parentFile.parentFile  // xen-fret/
val jniLibsDir = layout.projectDirectory.dir("src/main/jniLibs").asFile
val bindingsOutDir = layout.projectDirectory.dir("src/main/java").asFile
val rustLibName = "libxen_fret_uniffi.so"

// ── Resolve executables at execution time (not configuration time) ────────────
// Searches PATH + common Nix/Cargo install locations so it works both inside
// and outside `nix develop`.

fun findOnPath(name: String): String {
    val home = System.getenv("HOME") ?: ""
    val extraDirs = listOf(
        "$home/.cargo/bin",
        "$home/.nix-profile/bin",
        "/run/current-system/sw/bin",
        "/nix/var/nix/profiles/default/bin",
        "/opt/homebrew/bin",
        "/usr/local/bin",
    )
    val pathDirs = (System.getenv("PATH") ?: "").split(File.pathSeparatorChar)
    return (pathDirs + extraDirs)
        .distinct()
        .map { File(it, name) }
        .firstOrNull { it.canExecute() }
        ?.absolutePath
        ?: name  // fall back to bare name; let the OS report "not found"
}

// ── Rust cross-compilation tasks ──────────────────────────────────────────────

val buildRustLibrary by tasks.registering(Exec::class) {
    description = "Cross-compile uniffi-frontend-lib for Android via cargo-ndk"
    workingDir = repoRoot
    // cargo-ndk must be invoked as a cargo subcommand (`cargo ndk …`), not directly
    doFirst { executable = findOnPath("cargo") }
    args(
        "ndk",
        "-t", "arm64-v8a",
        "-t", "armeabi-v7a",
        "-t", "x86_64",
        "-o", jniLibsDir.absolutePath,
        "build", "--release", "-p", "uniffi-frontend-lib"
    )
    inputs.files(fileTree(repoRoot) {
        include("app/uniffi_frontend_lib/src/**", "app/common/src/**", "app/uniffi_frontend_lib/Cargo.toml")
    })
    outputs.dir(jniLibsDir)
}

val generateUniFFIBindings by tasks.registering(Exec::class) {
    description = "Generate Kotlin UniFFI bindings from the compiled arm64 library"
    dependsOn(buildRustLibrary)
    workingDir = repoRoot
    doFirst { executable = findOnPath("cargo") }
    args(
        "run",
        "--bin", "uniffi-bindgen",
        "--manifest-path", "app/uniffi_frontend_lib/Cargo.toml",
        "--",
        "generate",
        "--library", "target/aarch64-linux-android/release/$rustLibName",
        "--language", "kotlin",
        "--out-dir", bindingsOutDir.absolutePath
    )
    inputs.file(repoRoot.resolve("target/aarch64-linux-android/release/$rustLibName"))
    outputs.dir(bindingsOutDir.resolve("uniffi"))
}

tasks.named("preBuild") {
    dependsOn(generateUniFFIBindings)
}

// ── Android configuration ─────────────────────────────────────────────────────

android {
    namespace = "io.github.sintrastes.xenfret"
    compileSdk = 35

    defaultConfig {
        applicationId = "io.github.sintrastes.xenfret"
        minSdk = 24
        targetSdk = 35
        versionCode = 1
        versionName = "1.0"

        testInstrumentationRunner = "androidx.test.runner.AndroidJUnitRunner"
    }

    buildTypes {
        release {
            isMinifyEnabled = false
            proguardFiles(
                getDefaultProguardFile("proguard-android-optimize.txt"),
                "proguard-rules.pro"
            )
        }
    }
    compileOptions {
        sourceCompatibility = JavaVersion.VERSION_11
        targetCompatibility = JavaVersion.VERSION_11
    }
    kotlinOptions {
        jvmTarget = "11"
    }
    buildFeatures {
        compose = true
    }
    sourceSets {
        getByName("main") {
            jniLibs.srcDirs("src/main/jniLibs")
        }
    }
    packaging {
        jniLibs {
            // JNA ships its own native dispatcher; keep it
            keepDebugSymbols += "**/libjnidispatch.so"
        }
    }
}

dependencies {
    implementation(libs.androidx.core.ktx)
    implementation(libs.androidx.lifecycle.runtime.ktx)
    implementation(libs.androidx.activity.compose)
    implementation(platform(libs.androidx.compose.bom))
    implementation(libs.androidx.ui)
    implementation(libs.androidx.ui.graphics)
    implementation(libs.androidx.ui.tooling.preview)
    implementation(libs.androidx.material3)
    implementation(libs.androidx.lifecycle.viewmodel.compose)
    implementation(libs.androidx.navigation.compose)
    implementation(libs.kotlinx.coroutines.android)
    // @aar is required — the plain JAR doesn't include libjnidispatch.so for Android
    implementation("net.java.dev.jna:jna:${libs.versions.jna.get()}@aar")
    testImplementation(libs.junit)
    androidTestImplementation(libs.androidx.junit)
    androidTestImplementation(libs.androidx.espresso.core)
    androidTestImplementation(platform(libs.androidx.compose.bom))
    androidTestImplementation(libs.androidx.ui.test.junit4)
    debugImplementation(libs.androidx.ui.tooling)
    debugImplementation(libs.androidx.ui.test.manifest)
}

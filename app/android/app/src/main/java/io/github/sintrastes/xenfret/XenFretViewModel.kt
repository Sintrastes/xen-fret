package io.github.sintrastes.xenfret

import android.app.Application
import android.content.Context
import androidx.lifecycle.AndroidViewModel
import androidx.lifecycle.viewModelScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.Job
import kotlinx.coroutines.delay
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.flow.StateFlow
import kotlinx.coroutines.flow.asStateFlow
import kotlinx.coroutines.launch
import uniffi.xen_fret_uniffi.InstrumentRecord
import uniffi.xen_fret_uniffi.ScaleRecord
import uniffi.xen_fret_uniffi.TemperamentRecord
import uniffi.xen_fret_uniffi.TuningRecord
import uniffi.xen_fret_uniffi.XenFretApi
import uniffi.xen_fret_uniffi.xenFretInit
import java.io.File

enum class DiagramMode { Scale, Chord }

class XenFretViewModel(application: Application) : AndroidViewModel(application) {

    private val api: XenFretApi by lazy { XenFretApi() }

    private val _instruments = MutableStateFlow<List<InstrumentRecord>>(emptyList())
    val instruments: StateFlow<List<InstrumentRecord>> = _instruments.asStateFlow()

    private val _scales = MutableStateFlow<List<ScaleRecord>>(emptyList())
    val scales: StateFlow<List<ScaleRecord>> = _scales.asStateFlow()

    private val _tunings = MutableStateFlow<List<TuningRecord>>(emptyList())
    val tunings: StateFlow<List<TuningRecord>> = _tunings.asStateFlow()

    private val _temperaments = MutableStateFlow<List<TemperamentRecord>>(emptyList())
    val temperaments: StateFlow<List<TemperamentRecord>> = _temperaments.asStateFlow()

    private val _selectedInstrumentIdx = MutableStateFlow(0)
    val selectedInstrumentIdx: StateFlow<Int> = _selectedInstrumentIdx.asStateFlow()

    private val _selectedScaleIdx = MutableStateFlow(0)
    val selectedScaleIdx: StateFlow<Int> = _selectedScaleIdx.asStateFlow()

    private val _selectedTuningIdx = MutableStateFlow(0)
    val selectedTuningIdx: StateFlow<Int> = _selectedTuningIdx.asStateFlow()

    private val _key = MutableStateFlow(0)
    val key: StateFlow<Int> = _key.asStateFlow()

    private val _fretOffset = MutableStateFlow(0)
    val fretOffset: StateFlow<Int> = _fretOffset.asStateFlow()

    private val _mode = MutableStateFlow(DiagramMode.Scale)
    val mode: StateFlow<DiagramMode> = _mode.asStateFlow()

    private var renderJob: Job? = null

    // null = loading, empty = no instrument selected, non-empty = PNG bytes
    private val _diagramPng = MutableStateFlow<ByteArray?>(null)
    val diagramPng: StateFlow<ByteArray?> = _diagramPng.asStateFlow()

    private val _edo = MutableStateFlow(12)
    val edo: StateFlow<Int> = _edo.asStateFlow()

    init {
        // Trivial global write — must happen synchronously before any api access,
        // including from LaunchedEffect callbacks that fire before the IO coroutine runs.
        xenFretInit(application.filesDir.absolutePath)

        viewModelScope.launch(Dispatchers.IO) {
            refreshLists()
            regenerateDiagram()
        }
    }

    fun selectInstrument(idx: Int) {
        viewModelScope.launch(Dispatchers.IO) {
            api.selectInstrument(idx.toUInt())
            _selectedInstrumentIdx.value = idx
            refreshLists()
            regenerateDiagram()
        }
    }

    fun selectScale(idx: Int) {
        viewModelScope.launch(Dispatchers.IO) {
            api.selectScale(idx.toUInt())
            _selectedScaleIdx.value = idx
            regenerateDiagram()
        }
    }

    fun selectTuning(idx: Int) {
        viewModelScope.launch(Dispatchers.IO) {
            api.selectTuning(idx.toUInt())
            _selectedTuningIdx.value = idx
            regenerateDiagram()
        }
    }

    fun setMode(mode: DiagramMode) {
        viewModelScope.launch(Dispatchers.IO) {
            if (mode == DiagramMode.Scale) api.setDiagramModeScale() else api.setDiagramModeChord()
            _mode.value = mode
            regenerateDiagram()
        }
    }

    fun setKey(k: Int) {
        viewModelScope.launch(Dispatchers.IO) {
            api.setKey(k.toUInt())
            _key.value = k
            regenerateDiagram()
        }
    }

    fun setFretOffset(f: Int) {
        viewModelScope.launch(Dispatchers.IO) {
            api.setFretOffset(f.toUInt())
            _fretOffset.value = f
            regenerateDiagram()
        }
    }

    fun setDarkMode(dark: Boolean) {
        viewModelScope.launch(Dispatchers.IO) {
            api.setDarkMode(dark)
            regenerateDiagram()
        }
    }

    fun addInstrument(
        name: String,
        instrumentType: String,
        temperamentName: String,
        numStrings: Int,
        numFrets: Int,
    ) {
        viewModelScope.launch(Dispatchers.IO) {
            api.addInstrument(InstrumentRecord(
                name = name,
                instrumentType = instrumentType,
                temperamentName = temperamentName,
                numStrings = numStrings.toUInt(),
                numFrets = numFrets.toUInt(),
            ))
            val newIdx = api.getInstruments().size - 1
            api.selectInstrument(newIdx.toUInt())
            _selectedInstrumentIdx.value = newIdx
            api.save()
            refreshLists()
            regenerateDiagram()
        }
    }

    fun onDiagramTap(x: Double, y: Double, elemW: Double, elemH: Double) {
        viewModelScope.launch(Dispatchers.IO) {
            val hit = api.hitTestDiagram(elemW, elemH, x, y) ?: return@launch
            api.pushFlashStep(hit.absoluteStep)
            api.playStep(hit.absoluteStep)
            regenerateDiagram()
            delay(180)
            api.popFlashStep(hit.absoluteStep)
            regenerateDiagram()
        }
    }

    // ── Private helpers ───────────────────────────────────────────────────────

    private fun refreshLists() {
        val instruments = api.getInstruments()
        val scales = api.getScales()
        val tunings = api.getTunings()
        val temperaments = api.getTemperaments()
        _instruments.value = instruments
        _scales.value = scales
        _tunings.value = tunings
        _temperaments.value = temperaments

        val temperamentName = instruments.getOrNull(_selectedInstrumentIdx.value)?.temperamentName
        _edo.value = temperaments.firstOrNull { it.name == temperamentName }?.divisions?.toInt() ?: 12
    }

    private fun regenerateDiagram() {
        renderJob?.cancel()
        renderJob = viewModelScope.launch(Dispatchers.IO) {
            val png = api.generateDiagramPng()
            _diagramPng.value = png ?: ByteArray(0)
        }
    }
}

private fun extractFont(context: Context): String {
    val dest = File(context.filesDir, "Bravura.woff")
    if (!dest.exists()) {
        context.assets.open("Bravura.woff").use { input ->
            dest.outputStream().use { output -> input.copyTo(output) }
        }
    }
    return dest.absolutePath
}

package io.github.sintrastes.xenfret.ui.screens

import android.graphics.Bitmap
import android.graphics.BitmapFactory
import androidx.compose.foundation.Image
import androidx.compose.foundation.clickable
import androidx.compose.foundation.isSystemInDarkTheme
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.itemsIndexed
import androidx.compose.foundation.lazy.rememberLazyListState
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.ArrowDropDown
import androidx.compose.material.icons.filled.Edit
import androidx.compose.material3.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.asImageBitmap
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.unit.dp
import androidx.lifecycle.viewmodel.compose.viewModel
import io.github.sintrastes.xenfret.DiagramMode
import io.github.sintrastes.xenfret.XenFretViewModel
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.withContext

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun MainScreen(vm: XenFretViewModel = viewModel()) {
    val instruments by vm.instruments.collectAsState()
    val scales by vm.scales.collectAsState()
    val tunings by vm.tunings.collectAsState()
    val selectedInstrumentIdx by vm.selectedInstrumentIdx.collectAsState()
    val selectedScaleIdx by vm.selectedScaleIdx.collectAsState()
    val selectedTuningIdx by vm.selectedTuningIdx.collectAsState()
    val key by vm.key.collectAsState()
    val fretOffset by vm.fretOffset.collectAsState()
    val mode by vm.mode.collectAsState()
    val diagramPng by vm.diagramPng.collectAsState()
    val edo by vm.edo.collectAsState()
    val temperaments by vm.temperaments.collectAsState()

    val isDark = isSystemInDarkTheme()
    LaunchedEffect(isDark) { vm.setDarkMode(isDark) }

    var showAddInstrument by remember { mutableStateOf(false) }

    var showSheet by remember { mutableStateOf(false) }
    val sheetState = rememberModalBottomSheetState(skipPartiallyExpanded = false)

    Scaffold(
        topBar = {
            TopAppBar(
                title = { Text("Xen Fret") },
                colors = TopAppBarDefaults.topAppBarColors(
                    containerColor = MaterialTheme.colorScheme.primaryContainer,
                    titleContentColor = MaterialTheme.colorScheme.onPrimaryContainer,
                ),
            )
        },
        floatingActionButton = {
            FloatingActionButton(onClick = { showSheet = true }) {
                Icon(Icons.Default.Edit, contentDescription = "Controls")
            }
        },
    ) { innerPadding ->
        Box(
            modifier = Modifier
                .fillMaxSize()
                .padding(innerPadding),
            contentAlignment = Alignment.Center,
        ) {
            when {
                diagramPng == null -> CircularProgressIndicator()
                diagramPng!!.isEmpty() -> Text(
                    "No instrument selected.\nOpen controls to add one.",
                    style = MaterialTheme.typography.bodyMedium,
                    textAlign = androidx.compose.ui.text.style.TextAlign.Center,
                )
                else -> {
                    val bitmap = rememberPngBitmap(diagramPng)
                    if (bitmap != null) {
                        Image(
                            bitmap = bitmap.asImageBitmap(),
                            contentDescription = "Fretboard diagram",
                            contentScale = ContentScale.Fit,
                            modifier = Modifier
                                .fillMaxSize()
                                .padding(16.dp),
                        )
                    } else {
                        Text("Unable to render diagram", style = MaterialTheme.typography.bodyMedium)
                    }
                }
            }
        }
    }

    if (showSheet) {
        ModalBottomSheet(
            onDismissRequest = { showSheet = false },
            sheetState = sheetState,
        ) {
            ControlsPanel(
                instruments = instruments.map { it.name },
                scales = scales.map { it.name },
                tunings = tunings.map { it.name },
                selectedInstrumentIdx = selectedInstrumentIdx,
                selectedScaleIdx = selectedScaleIdx,
                selectedTuningIdx = selectedTuningIdx,
                key = key,
                fretOffset = fretOffset,
                edo = edo,
                mode = mode,
                onInstrumentSelected = vm::selectInstrument,
                onScaleSelected = vm::selectScale,
                onTuningSelected = vm::selectTuning,
                onKeyChanged = vm::setKey,
                onFretOffsetChanged = vm::setFretOffset,
                onModeChanged = vm::setMode,
                onAddInstrument = { showAddInstrument = true },
            )
        }
    }

    if (showAddInstrument) {
        AddInstrumentDialog(
            temperaments = temperaments,
            onDismiss = { showAddInstrument = false },
            onConfirm = { name, type, temperamentName, numStrings, numFrets ->
                vm.addInstrument(name, type, temperamentName, numStrings, numFrets)
                showAddInstrument = false
            },
        )
    }
}

@OptIn(ExperimentalMaterial3Api::class)
@Composable
private fun ControlsPanel(
    instruments: List<String>,
    scales: List<String>,
    tunings: List<String>,
    selectedInstrumentIdx: Int,
    selectedScaleIdx: Int,
    selectedTuningIdx: Int,
    key: Int,
    fretOffset: Int,
    edo: Int,
    mode: DiagramMode,
    onInstrumentSelected: (Int) -> Unit,
    onScaleSelected: (Int) -> Unit,
    onTuningSelected: (Int) -> Unit,
    onKeyChanged: (Int) -> Unit,
    onFretOffsetChanged: (Int) -> Unit,
    onModeChanged: (DiagramMode) -> Unit,
    onAddInstrument: () -> Unit,
) {
    Column(
        modifier = Modifier
            .fillMaxWidth()
            .verticalScroll(rememberScrollState())
            .padding(horizontal = 16.dp, vertical = 8.dp)
            .navigationBarsPadding(),
        verticalArrangement = Arrangement.spacedBy(16.dp),
    ) {
        // Instrument
        Row(
            modifier = Modifier.fillMaxWidth(),
            horizontalArrangement = Arrangement.SpaceBetween,
            verticalAlignment = androidx.compose.ui.Alignment.CenterVertically,
        ) {
            Text("Instrument", style = MaterialTheme.typography.labelLarge)
            TextButton(onClick = onAddInstrument) { Text("+ Add") }
        }
        if (instruments.isNotEmpty()) {
            LabeledDropdown(
                label = null,
                options = instruments,
                selectedIdx = selectedInstrumentIdx,
                onSelected = onInstrumentSelected,
            )
        }

        HorizontalDivider()

        // Scale / Chord mode toggle
        SingleChoiceSegmentedButtonRow(modifier = Modifier.fillMaxWidth()) {
            SegmentedButton(
                shape = SegmentedButtonDefaults.itemShape(index = 0, count = 2),
                onClick = { onModeChanged(DiagramMode.Scale) },
                selected = mode == DiagramMode.Scale,
                label = { Text("Scale") },
            )
            SegmentedButton(
                shape = SegmentedButtonDefaults.itemShape(index = 1, count = 2),
                onClick = { onModeChanged(DiagramMode.Chord) },
                selected = mode == DiagramMode.Chord,
                label = { Text("Chord") },
            )
        }

        // Scale / Chord selector
        LabeledDropdown(
            label = if (mode == DiagramMode.Scale) "Scale" else "Chord",
            options = scales,
            selectedIdx = selectedScaleIdx,
            onSelected = onScaleSelected,
        )

        HorizontalDivider()

        // Tuning
        LabeledDropdown(
            label = "Tuning",
            options = tunings,
            selectedIdx = selectedTuningIdx,
            onSelected = onTuningSelected,
        )

        HorizontalDivider()

        // Key slider
        LabeledSlider(
            label = "Key: $key",
            value = key.toFloat(),
            valueRange = 0f..(maxOf(edo - 1, 1)).toFloat(),
            steps = maxOf(edo - 2, 0),
            onValueChangeFinished = { onKeyChanged(key) },
            onValueChange = { onKeyChanged(it.toInt()) },
        )

        // Fret offset slider
        LabeledSlider(
            label = "Fret Offset: $fretOffset",
            value = fretOffset.toFloat(),
            valueRange = 0f..24f,
            steps = 23,
            onValueChangeFinished = { onFretOffsetChanged(fretOffset) },
            onValueChange = { onFretOffsetChanged(it.toInt()) },
        )

        Spacer(Modifier.height(8.dp))
    }
}

@Composable
private fun LabeledDropdown(
    label: String?,
    options: List<String>,
    selectedIdx: Int,
    onSelected: (Int) -> Unit,
) {
    var showDialog by remember { mutableStateOf(false) }
    val selectedLabel = options.getOrElse(selectedIdx) { "" }

    Column(verticalArrangement = Arrangement.spacedBy(4.dp)) {
        if (label != null) Text(label, style = MaterialTheme.typography.labelLarge)
        Box {
            OutlinedTextField(
                value = selectedLabel,
                onValueChange = {},
                readOnly = true,
                enabled = false,
                trailingIcon = { Icon(Icons.Default.ArrowDropDown, contentDescription = null) },
                colors = OutlinedTextFieldDefaults.colors(
                    disabledTextColor = MaterialTheme.colorScheme.onSurface,
                    disabledBorderColor = MaterialTheme.colorScheme.outline,
                    disabledTrailingIconColor = MaterialTheme.colorScheme.onSurfaceVariant,
                ),
                modifier = Modifier.fillMaxWidth(),
            )
            Box(modifier = Modifier.matchParentSize().clickable { showDialog = true })
        }
    }

    if (showDialog) {
        val listState = rememberLazyListState()
        LaunchedEffect(Unit) {
            if (selectedIdx > 0) listState.scrollToItem(selectedIdx)
        }
        AlertDialog(
            onDismissRequest = { showDialog = false },
            title = label?.let { { Text(it) } },
            text = {
                LazyColumn(state = listState) {
                    itemsIndexed(options) { idx, name ->
                        TextButton(
                            onClick = { onSelected(idx); showDialog = false },
                            modifier = Modifier.fillMaxWidth(),
                        ) {
                            Text(
                                text = name,
                                color = if (idx == selectedIdx)
                                    MaterialTheme.colorScheme.primary
                                else
                                    MaterialTheme.colorScheme.onSurface,
                                modifier = Modifier.fillMaxWidth(),
                            )
                        }
                    }
                }
            },
            confirmButton = {},
            dismissButton = {
                TextButton(onClick = { showDialog = false }) { Text("Cancel") }
            },
        )
    }
}

@Composable
private fun LabeledSlider(
    label: String,
    value: Float,
    valueRange: ClosedFloatingPointRange<Float>,
    steps: Int,
    onValueChange: (Float) -> Unit,
    onValueChangeFinished: () -> Unit,
) {
    Column(verticalArrangement = Arrangement.spacedBy(4.dp)) {
        Text(label, style = MaterialTheme.typography.labelLarge)
        Slider(
            value = value,
            onValueChange = onValueChange,
            onValueChangeFinished = onValueChangeFinished,
            valueRange = valueRange,
            steps = steps,
            modifier = Modifier.fillMaxWidth(),
        )
    }
}

@Composable
private fun rememberPngBitmap(png: ByteArray?): Bitmap? {
    var bitmap by remember(png) { mutableStateOf<Bitmap?>(null) }
    LaunchedEffect(png) {
        if (png == null || png.isEmpty()) return@LaunchedEffect
        bitmap = withContext(Dispatchers.Default) {
            BitmapFactory.decodeByteArray(png, 0, png.size)
        }
    }
    return bitmap
}

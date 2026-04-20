package io.github.sintrastes.xenfret.ui.screens

import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.text.KeyboardOptions
import androidx.compose.foundation.verticalScroll
import androidx.compose.material3.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.input.KeyboardType
import androidx.compose.ui.unit.dp
import uniffi.xen_fret_uniffi.TemperamentRecord

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun AddInstrumentDialog(
    temperaments: List<TemperamentRecord>,
    onDismiss: () -> Unit,
    onConfirm: (name: String, type: String, temperamentName: String, numStrings: Int, numFrets: Int) -> Unit,
) {
    var name by remember { mutableStateOf("") }
    var instrumentType by remember { mutableStateOf("Guitar") }
    var selectedTemperamentIdx by remember { mutableStateOf(0) }
    var numStringsText by remember { mutableStateOf("6") }
    var numFretsText by remember { mutableStateOf("24") }
    var typeExpanded by remember { mutableStateOf(false) }
    var temperamentExpanded by remember { mutableStateOf(false) }

    val instrumentTypes = listOf("Guitar", "Bass", "Banjo", "Mandolin", "Ukulele", "Oud", "Other")
    val temperamentName = temperaments.getOrNull(selectedTemperamentIdx)?.name ?: ""
    val numStrings = numStringsText.toIntOrNull()?.coerceIn(1, 12) ?: 6
    val numFrets = numFretsText.toIntOrNull()?.coerceIn(1, 36) ?: 24
    val isValid = name.isNotBlank() && temperamentName.isNotEmpty()

    AlertDialog(
        onDismissRequest = onDismiss,
        title = { Text("Add Instrument") },
        text = {
            Column(
                modifier = Modifier
                    .verticalScroll(rememberScrollState())
                    .fillMaxWidth(),
                verticalArrangement = Arrangement.spacedBy(12.dp),
            ) {
                OutlinedTextField(
                    value = name,
                    onValueChange = { name = it },
                    label = { Text("Name") },
                    singleLine = true,
                    modifier = Modifier.fillMaxWidth(),
                )

                // Instrument type dropdown
                ExposedDropdownMenuBox(
                    expanded = typeExpanded,
                    onExpandedChange = { typeExpanded = !typeExpanded },
                ) {
                    OutlinedTextField(
                        value = instrumentType,
                        onValueChange = {},
                        readOnly = true,
                        label = { Text("Type") },
                        trailingIcon = { ExposedDropdownMenuDefaults.TrailingIcon(typeExpanded) },
                        modifier = Modifier
                            .menuAnchor()
                            .fillMaxWidth(),
                    )
                    ExposedDropdownMenu(
                        expanded = typeExpanded,
                        onDismissRequest = { typeExpanded = false },
                    ) {
                        instrumentTypes.forEach { t ->
                            DropdownMenuItem(
                                text = { Text(t) },
                                onClick = { instrumentType = t; typeExpanded = false },
                            )
                        }
                    }
                }

                // Temperament dropdown
                ExposedDropdownMenuBox(
                    expanded = temperamentExpanded,
                    onExpandedChange = { temperamentExpanded = !temperamentExpanded },
                ) {
                    OutlinedTextField(
                        value = temperamentName,
                        onValueChange = {},
                        readOnly = true,
                        label = { Text("Temperament") },
                        trailingIcon = { ExposedDropdownMenuDefaults.TrailingIcon(temperamentExpanded) },
                        modifier = Modifier
                            .menuAnchor()
                            .fillMaxWidth(),
                    )
                    ExposedDropdownMenu(
                        expanded = temperamentExpanded,
                        onDismissRequest = { temperamentExpanded = false },
                    ) {
                        temperaments.forEachIndexed { idx, t ->
                            DropdownMenuItem(
                                text = { Text(t.name) },
                                onClick = { selectedTemperamentIdx = idx; temperamentExpanded = false },
                            )
                        }
                    }
                }

                Row(
                    modifier = Modifier.fillMaxWidth(),
                    horizontalArrangement = Arrangement.spacedBy(8.dp),
                ) {
                    OutlinedTextField(
                        value = numStringsText,
                        onValueChange = { numStringsText = it.filter(Char::isDigit).take(2) },
                        label = { Text("Strings") },
                        singleLine = true,
                        keyboardOptions = KeyboardOptions(keyboardType = KeyboardType.Number),
                        modifier = Modifier.weight(1f),
                    )
                    OutlinedTextField(
                        value = numFretsText,
                        onValueChange = { numFretsText = it.filter(Char::isDigit).take(2) },
                        label = { Text("Frets") },
                        singleLine = true,
                        keyboardOptions = KeyboardOptions(keyboardType = KeyboardType.Number),
                        modifier = Modifier.weight(1f),
                    )
                }
            }
        },
        confirmButton = {
            TextButton(
                onClick = { onConfirm(name.trim(), instrumentType, temperamentName, numStrings, numFrets) },
                enabled = isValid,
            ) { Text("Add") }
        },
        dismissButton = {
            TextButton(onClick = onDismiss) { Text("Cancel") }
        },
    )
}

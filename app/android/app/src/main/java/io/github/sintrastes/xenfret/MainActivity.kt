package io.github.sintrastes.xenfret

import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.activity.enableEdgeToEdge
import androidx.lifecycle.viewmodel.compose.viewModel
import io.github.sintrastes.xenfret.ui.screens.MainScreen
import io.github.sintrastes.xenfret.ui.theme.XenFretTheme

class MainActivity : ComponentActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        enableEdgeToEdge()
        setContent {
            XenFretTheme {
                val vm: XenFretViewModel = viewModel()
                MainScreen(vm)
            }
        }
    }
}

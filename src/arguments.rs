use std::{collections::{HashSet, HashMap}, env::Args, fmt::Debug};

pub type CheckSettingFn = fn(String) -> Result<String, String>;
pub struct Arguments {
    pub collected_input: Vec<String>,
    pub collected_settings: HashMap<String, String>,
    pub collected_flags: HashSet<String>,

    pub settings: HashMap<String, Option<CheckSettingFn>>,
    pub flags: HashSet<String>,
    pub check_settings: bool,
    pub check_flags: bool,
}
impl Arguments {
    pub fn new() -> Self {
        Self {
            collected_input: vec![], collected_settings: HashMap::new(), collected_flags: HashSet::new(),
            settings: HashMap::new(), flags: HashSet::new(),
            check_settings: false, check_flags: false
        }
    }
    pub fn new_checked() -> Self {
        Self {
            collected_input: vec![], collected_settings: HashMap::new(), collected_flags: HashSet::new(),
            settings: HashMap::new(), flags: HashSet::new(),
            check_settings: true, check_flags: true
        }
    }
    pub fn args(mut self, args: Args) -> Result<Self, String> {
        self.collect(args)?;
        Ok(self)
    }
    pub fn setting<S: ToString>(mut self, setting: S) -> Self {
        self.create_setting(setting);
        self
    }
    pub fn settings<S: ToString>(mut self, settings: Vec<S>) -> Self {
        self.create_settings(settings);
        self
    }
    pub fn setting_checked<S: ToString>(mut self, setting: S, func: CheckSettingFn) -> Self {
        self.create_setting_checked(setting, func);
        self
    }
    pub fn settings_checked<S: ToString>(mut self, settings: Vec<(S, CheckSettingFn)>) -> Self {
        self.create_settings_checked(settings);
        self
    }
    pub fn flag<S: ToString>(mut self, flag: S) -> Self {
        self.create_flag(flag);
        self
    }
    pub fn flags<S: ToString + Debug>(mut self, flags: Vec<S>) -> Self {
        self.create_flags(flags);
        self
    }

    pub fn create_flag<S: ToString>(&mut self, flag: S) {
        self.flags.insert(flag.to_string());
    }
    pub fn create_flags<S: ToString>(&mut self, flags: Vec<S>) {
        for flag in flags {
            self.create_flag(flag);
        }
    }
    pub fn create_setting<S: ToString>(&mut self, setting: S) {
        self.settings.insert(setting.to_string(), None);
    }
    pub fn create_settings<S: ToString>(&mut self, settings: Vec<S>) {
        for setting in settings {
            self.create_setting(setting);
        }
    }
    pub fn create_setting_checked<S: ToString>(&mut self, setting: S, func: CheckSettingFn) {
        self.settings.insert(setting.to_string(), Some(func));
    }
    pub fn create_settings_checked<S: ToString>(&mut self, settings: Vec<(S, CheckSettingFn)>) {
        for (setting, func) in settings {
            self.create_setting_checked(setting, func);
        }
    }

    pub fn collect(&mut self, mut args: Args) -> Result<(), String> {
        args.next();
        while let Some(arg) = args.next() {
            if arg.starts_with("--") {
                let Some(flag) = arg.get(2..) else { continue; };
                if self.check_flags {
                    if !self.flags.contains(flag) {
                        return Err(format!("invalid flag {flag:?}"))
                    }
                }
                self.collected_flags.insert(flag.to_string());
                continue;
            }
            if arg.starts_with("-") {
                let Some(setting) = arg.get(1..) else { continue; };
                if self.check_settings {
                    if !self.settings.contains_key(setting) {
                        return Err(format!("invalid setting {setting:?}"))
                    }
                }
                let mut setting = setting.to_string();
                if let Some(arg) = args.next() {
                    if self.check_settings {
                        if let Some(func) = self.settings.get(&setting).unwrap() {
                            setting = func(setting.to_string())?;
                        }
                    }
                    self.collected_settings.insert(setting, arg);
                } else {
                    return Err(format!("expected value after setting {setting:?}"))
                }
                continue;
            }
            self.collected_input.push(arg);
        }
        Ok(())
    }

    pub fn next_input(&mut self) -> Option<String> {
        if self.collected_input.len() > 0 { Some(self.collected_input.remove(0)) } else { None }
    }
    pub fn next_input_ref(&self) -> Option<&String> {
        self.collected_input.first()
    }
    pub fn get_flag<S: ToString>(&self, flag: S) -> bool {
        self.collected_flags.contains(&flag.to_string())
    }
    pub fn pop_setting<S: ToString>(&mut self, setting: S) -> Option<String> {
        self.collected_settings.remove(&setting.to_string())
    }
    pub fn get_setting<S: ToString>(&self, setting: S) -> Option<&String> {
        self.collected_settings.get(&setting.to_string())
    }
}
def process_data(self, **kwargs):
        """
        Process data according to parameters in config.

        Checks certain parameters to make sure the limits make sense.
        Will accept silent=True kwarg to suppress printing.
        :return: None
        """
        tstart = time.perf_counter()
        self.export_config()
        
        # Config low m/z
        # config high m/z
        
        try:
            float(self.config.minmz)
        except ValueError:
            self.config.minmz = np.amin(self.data.rawdata[:, 0])

        try:
            float(self.config.maxmz)
        except ValueError:
            self.config.maxmz = np.amax(self.data.rawdata[:, 0])
        # print("Min MZ: ", self.config.minmz, "Max MZ: ", self.config.maxmz)
        
        # imflag
        if self.config.imflag == 1:
            
            # config min dt
            try:
                float(self.config.mindt)
            except ValueError:
                self.config.mindt = np.amin(self.data.rawdata3[:, 1])
            
            # config max dt
            try:
                float(self.config.maxdt)
            except ValueError:
                self.config.maxdt = np.amax(self.data.rawdata3[:, 1])

        if self.check_badness() == 1:
            print("Badness found, aborting data prep")
            return 1
        
        # config min dt
        if self.config.imflag == 0:
            self.data.data2 = ud.dataprep(self.data.rawdata, self.config)
            if "scramble" in kwargs:
                if kwargs["scramble"]:
                    # np.random.shuffle(self.data.data2[:, 1])
                    self.data.data2[:, 1] = np.abs(
                        np.random.normal(0, 100 * np.amax(self.data.data2[:, 1]), len(self.data.data2)))
                    self.data.data2[:, 1] /= np.amax(self.data.data2[:, 1])
                    print("Added noise to data")
            ud.dataexport(self.data.data2, self.config.infname)
        else:
            tstart2 = time.perf_counter()
            mz, dt, i3 = IM_func.process_data_2d(self.data.rawdata3[:, 0], self.data.rawdata3[:, 1],
                                                 self.data.rawdata3[:, 2],
                                                 self.config)
            tend = time.perf_counter()
            if "silent" not in kwargs or not kwargs["silent"]:
                print("Time: %.2gs" % (tend - tstart2))
            self.data.data3 = np.transpose([np.ravel(mz), np.ravel(dt), np.ravel(i3)])
            self.data.data2 = np.transpose([np.unique(mz), np.sum(i3, axis=1)])
            ud.dataexportbin(self.data.data3, self.config.infname)
            pass

        self.config.procflag = 1
        tend = time.perf_counter()
        if "silent" not in kwargs or not kwargs["silent"]:
            print("Data Prep Time: %.2gs" % (tend - tstart))
        # self.get_spectrum_peaks()
        pass


def run_unidec(self, silent=False, efficiency=False):
        """
        Runs unidec.

        Checks that everything is set to go and then places external call to:
            self.config.UniDecPath for MS
            self.config.UniDecIMPath for IM-MS

        If successful, calls self.unidec_imports()
        If not, prints the error code.
        :param silent: If True, it will suppress printing the output from unidec
        :param efficiency: Passed to self.unidec_imports()
        :return: out (stdout from external unidec call)
        """
        # Check to make sure everything is in order
        if self.config.procflag == 0:
            print("Need to process data first. Processing...")
            self.process_data()
        if self.check_badness() == 1:
            print("Badness found, aborting unidec run")
            return 1
        if self.config.doubledec:
            kpath = self.config.kernel
            try:
                with open(kpath, "r") as f:
                    pass
            except (IOError, FileNotFoundError) as err:
                print("Could not open kernel file.\nPlease select a valid kernel file to use DoubleDec")
                return 0
        # Export Config and Call
        self.export_config()
        tstart = time.perf_counter()

        out = ud.unidec_call(self.config, silent=silent)

        tend = time.perf_counter()
        self.config.runtime = (tend - tstart)
        if not silent:
            print("unidec run %.2gs" % self.config.runtime)
        # Import Results if Successful
        if out == 0:
            self.unidec_imports(efficiency)
            if not silent:
                print("File Name: ", self.config.filename, "R Squared: ", self.config.error)
            return out
        else:
            print("unidec Run Error:", out)
            return out
          
def pick_peaks(self, calc_dscore=True):
        """
        Detect, Normalize, and Output Peaks
        :return: None
        """
        self.export_config()
        # Detect Peaks and Normalize
        peaks = ud.peakdetect(self.data.massdat, self.config)
        if len(peaks) > 0:
            self.setup_peaks(peaks)
            if calc_dscore:
                try:
                    self.dscore()
                except:
                    pass
        else:
            print("No peaks detected", peaks, self.config.peakwindow, self.config.peakthresh)
            print("Mass Data:", self.data.massdat)
        return peaks

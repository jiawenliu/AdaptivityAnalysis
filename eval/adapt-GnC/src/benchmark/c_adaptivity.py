import math
import numpy as np
import matplotlib.pyplot as plt

from enum import Enum
import sys
sys.path.append("..")

import cw_funcs as cw
import helper_funcs as hf
import strategies as stg
import mechanisms as mech


DATA_SIZE = 1000
CARDINALITY = 1000
MAX_QUERY_NUM = 1000
MAX_EPOCH = 100
MEAN = 0.1


def c_adaptivity(strategy, mechanism, epoch = MAX_EPOCH):
    l, queried_set = 0, []
    q = strategy.next_query()
    while q and l < epoch:
        if q is None:
            break
        r = mechanism.get_answer(q["query"])
        if r[0]["answer"] is not None:
            queried_set.append((r[0]["answer"] * 2.0 - 1))
            pre_ans = [{"answer": np.sign(np.sum((queried_set)))}]
            q = strategy.next_query(pre_ans)
        else:
            q = None
            break
        l = l + 1
    return 




def eval_c_adaptivity(n = DATA_SIZE, cardinality = CARDINALITY, q_max = MAX_QUERY_NUM, mechanism = mech.Mechanism()):
    strategy = stg.Strategy(n, q_mean = MEAN, ada_freq = {"method": "c_adaptivity", "method_param": q_max}, q_max = q_max, cardinality = cardinality)
    mechanism.reset()
    mechanism.add_data({'data': strategy.gen_data()})

    c_adaptivity(strategy, mechanism)
    q_done = len(strategy.mech_ans_list)
    se = np.square(np.subtract(strategy.true_ans_list[:q_done], strategy.mech_ans_list))
    mse = [se[:i].mean() for i in range(q_done)]

    return np.sqrt(mse)

n = 100
dimension = 100
q_max = 100
runs = 10

stepped_q_max = range(q_max/2, q_max, 10)

beta, tau = 0.05, 1.0
gaussian_sigma, laplace_sigma = 0.7, 0.7
hold_frac, threshold, check_data_frac = 0.5, 0.5, 0.05

repeated_query_sub_delta = 0.1

Baseline = mech.Mechanism()
Baseline.add_params(beta=beta, tau=tau, check_for_width=None)
Baseline_rmse = np.mean([eval_c_adaptivity(n, dimension, q_max, Baseline) for _ in range(runs)], axis = 0)

DataSplit = mech.Mechanism(q_max)
DataSplit.add_params(beta=beta, tau=tau)
DataSplit_rmse = eval_c_adaptivity(n*10, dimension, q_max, DataSplit)

Thresh = mech.Thresholdout_Mechanism(hold_frac=hold_frac, threshold=threshold, sigma=laplace_sigma)
Thresh.add_params(beta=beta, tau=tau, check_for_width=None)
# Thresh_rmse = [eval_c_adaptivity(n, dimension, q_max, Thresh).mean() for q_max in stepped_q_max]
# Thresh_rmse = eval_c_adaptivity(n, dimension, q_max, Thresh)
Thresh_rmse = np.mean([eval_c_adaptivity(n, dimension, q_max, Thresh) for _ in range(runs)], axis = 0)


Gauss = mech.Gaussian_Mechanism(sigma=gaussian_sigma)
Gauss.add_params(beta=beta, tau=tau, check_for_width=None)
# Gauss_rmse = [eval_c_adaptivity(n, dimension, q_max, Gauss).mean() for q_max in stepped_q_max]
# Gauss_rmse = eval_c_adaptivity(n, dimension, q_max, Gauss)
Gauss_rmse = np.mean([eval_c_adaptivity(n, dimension, q_max, Gauss) for _ in range(runs)], axis = 0)



print(Baseline_rmse, DataSplit_rmse, Gauss_rmse, Thresh_rmse)
print(Baseline_rmse[1:].mean(), DataSplit_rmse[1:].mean(), Gauss_rmse[1:].mean(), Thresh_rmse[1:].mean())


plt.figure()
x_list = range(10, 101, 10)

plt.plot(Baseline_rmse, 'g', label= "empirical")
# plt.plot(x_list, DataSplit_rmse, 'y', label= "DataSplit")
plt.plot(Thresh_rmse, 'y', label= "Threshold - Adaptfun")
plt.plot(Gauss_rmse, 'r', label= "Gaussian ")
# plt.plot(x_list, GnC_gauss_rmse, 'm', label= "GnC_gauss")
# plt.plot(x_list, GnC_thresh_rmse, 'c', label= "GnC_thresh")
# plt.plot(x_list, GnC_DataSplit_rmse, label= "GnC_DataSplit")
plt.xlabel("Queries")
plt.ylabel("RMSE (Generalization Error) for adaptive queries")
plt.legend()
plt.grid()
plt.savefig("../../plots/c_adaptivity-test.png")
plt.show()


'''
(array([       nan, 1.3       , 1.2409566 , 1.50376543, 1.35389313,
       1.30961168, 1.27841376, 1.36947703, 1.33582744, 1.36207144,
       1.3835785 , 1.4150454 , 1.38135837, 1.39842507, 1.37929202,
       1.37159996, 1.38521558, 1.40258809, 1.42697535, 1.44946954,
       1.44656271, 1.47146963, 1.48137296, 1.46635494, 1.470027  ,
       1.46750731, 1.46554335, 1.46335553, 1.45217732, 1.44146275,
       1.44497952, 1.44864887, 1.44730099, 1.45903485, 1.46574882,
       1.47164172, 1.47407556, 1.47190877, 1.47399929, 1.47234168,
       1.47496247, 1.47006022, 1.47869524, 1.48350934, 1.48211467,
       1.4864633 , 1.48512225, 1.48069834, 1.4736569 , 1.47219506,
       1.47110647, 1.46740554, 1.46364501, 1.46008484, 1.464259  ,
       1.46319059, 1.46984574, 1.46868104, 1.47231172, 1.47371298,
       1.47265733, 1.4715118 , 1.47300785, 1.46787482, 1.46926871,
       1.46632359, 1.46581019, 1.46938047, 1.46654285, 1.46772012,
       1.47284892, 1.469977  , 1.47087257, 1.46629351, 1.46383785,
       1.46317092, 1.46436224, 1.46198207, 1.45590169, 1.45539259,
       1.45838541, 1.45782919, 1.45717194, 1.45660128, 1.45785293,
       1.45586588, 1.45687709, 1.45629234, 1.45890868, 1.45841379,
       1.45786618, 1.4589172 , 1.45689508, 1.45803469, 1.46055324,
       1.46004246, 1.46228576, 1.46318011, 1.46259816, 1.46488979]), array([       nan, 1.        , 0.70710678, 0.57735027, 1.11803399,
       1.34164079, 1.47196014, 1.55838744, 1.45773797, 1.52752523,
       1.44913767, 1.38169856, 1.44337567, 1.38675049, 1.33630621,
       1.29099445, 1.25      , 1.30609431, 1.3540064 , 1.39548143,
       1.43178211, 1.46385011, 1.43019388, 1.4596009 , 1.42886902,
       1.4       , 1.37281295, 1.4010578 , 1.37581145, 1.40196906,
       1.37840488, 1.35599029, 1.38067013, 1.35958996, 1.33944677,
       1.32017315, 1.30170828, 1.32542701, 1.34751233, 1.36813555,
       1.35092561, 1.3704192 , 1.38873015, 1.37248713, 1.35680105,
       1.34164079, 1.32697761, 1.31278492, 1.29903811, 1.31707778,
       1.33416641, 1.32102159, 1.33733374, 1.32465731, 1.31233465,
       1.32801972, 1.31610898, 1.30451308, 1.31961332, 1.30838236,
       1.32287566, 1.33674427, 1.32592025, 1.31535498, 1.30503831,
       1.31850731, 1.33143805, 1.34386389, 1.35581536, 1.36732045,
       1.37840488, 1.38909232, 1.39940464, 1.38978662, 1.39980694,
       1.39044357, 1.38126563, 1.39106611, 1.40054934, 1.4097311 ,
       1.4186261 , 1.40984195, 1.40121898, 1.39275232, 1.40152978,
       1.39326109, 1.38513705, 1.3937463 , 1.38580466, 1.39420953,
       1.40237893, 1.39465227, 1.38705193, 1.3795745 , 1.38763537,
       1.3803127 , 1.38819427, 1.38102009, 1.38873015]), array([       nan, 1.        , 0.90710678, 0.9039489 , 1.13889051,
       1.2020022 , 1.26790461, 1.26728953, 1.34604909, 1.40989401,
       1.37695152, 1.36021184, 1.35019849, 1.34306832, 1.35939046,
       1.36393761, 1.35173681, 1.34661163, 1.38576664, 1.39905346,
       1.40747379, 1.40411024, 1.40601695, 1.4053952 , 1.40575743,
       1.40747563, 1.40849938, 1.41221825, 1.41649265, 1.42671657,
       1.4083724 , 1.40407305, 1.39577544, 1.39200382, 1.39623604,
       1.40940438, 1.40562028, 1.40599225, 1.40933468, 1.4128017 ,
       1.40640742, 1.41366476, 1.41053896, 1.40769295, 1.41105193,
       1.41102336, 1.4111507 , 1.41434008, 1.40888488, 1.41171347,
       1.40603638, 1.40617522, 1.4088102 , 1.41411905, 1.41932882,
       1.41137522, 1.41420635, 1.4116126 , 1.41435527, 1.40951555,
       1.40976191, 1.40529778, 1.40328429, 1.40341484, 1.40104673,
       1.39468371, 1.39276242, 1.39113363, 1.38958557, 1.38577363,
       1.38624038, 1.38654071, 1.38907241, 1.38753378, 1.39565529,
       1.39397276, 1.39232766, 1.39276188, 1.39299666, 1.39158351,
       1.39003794, 1.38678554, 1.38353624, 1.3855063 , 1.38572174,
       1.38426006, 1.38301291, 1.3833185 , 1.37868778, 1.38069883,
       1.38110444, 1.38293877, 1.38339302, 1.38232888, 1.37803857,
       1.37535551, 1.38043981, 1.37937857, 1.3768345 , 1.37858806]), array([       nan, 0.8       , 1.15262387, 1.16875171, 1.31585541,
       1.2954764 , 1.26709597, 1.37263834, 1.33939733, 1.32605326,
       1.30481168, 1.30953089, 1.32351145, 1.31267086, 1.31460236,
       1.33254448, 1.35318551, 1.39205939, 1.37714413, 1.39339468,
       1.40260526, 1.43009289, 1.42623108, 1.44012179, 1.44021089,
       1.42871296, 1.43021788, 1.43022395, 1.43439689, 1.43439854,
       1.42025472, 1.42091647, 1.42058752, 1.4209235 , 1.42495237,
       1.42907174, 1.44391136, 1.4399714 , 1.44251784, 1.43475889,
       1.43098743, 1.43376778, 1.4468375 , 1.43306082, 1.43567648,
       1.43468839, 1.44340797, 1.44872862, 1.44202468, 1.43956206,
       1.43656027, 1.43120384, 1.42528297, 1.43306253, 1.43058711,
       1.43267915, 1.43461933, 1.43193695, 1.43408079, 1.4313705 ,
       1.42874687, 1.42170909, 1.41490773, 1.41732125, 1.4104882 ,
       1.41259137, 1.40816273, 1.40831088, 1.4124734 , 1.40815933,
       1.41013618, 1.41198997, 1.41177125, 1.41360874, 1.41358596,
       1.41392319, 1.41396869, 1.41970954, 1.42138437, 1.42130658,
       1.41930306, 1.41560783, 1.41193309, 1.41031181, 1.40855241,
       1.41179722, 1.41666698, 1.41695684, 1.42025959, 1.41895259,
       1.41889966, 1.42041069, 1.41863014, 1.42001701, 1.41852755,
       1.42023017, 1.41871908, 1.4186622 , 1.41710125, 1.41443426]))
       '''